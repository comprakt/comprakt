#![feature(proc_macro_hygiene, decl_macro)]

//! This is a browser-based graphical debugger for FIRM graphs.
//!
//! # Usage
//!
//! All interaction with the debugger is done through the `breakpoint!` macro.
//! The macro can be called in two variants:
//!
//! `breakpoint!(label:&str,compiler_state:firm::Program|Graph)` will
//! print all graphs with each node labeled by its kind and id.
//!
//! `breakpoint!(label:&str,compiler_state:firm::Program|Graph, label_maker)`
//! can be used to highlight some nodes and to add additional information.
//! `label_maker` can be a `Fn(Node) -> Label` or just a `HashMap<Node,Label>`.
//!
//! So for example, to highlight the currently visited node and to add
//! the current lattice value assigned to each node, you can write:
//!
//! ```ignore
//! // bring debugging into scope
//! use debugging;
//! // assume a context similar to:
//! // let values :HashMap<Node,Tarval> = HashMap::new();
//! // while let Some(cur) = next_node()
//! breakpoint!("Constant Folding: iteration", self.graph, &|node| {
//!     let mut label = default_label(node);
//!
//!     if let Some(tarval) = self.values.get(&node) {
//!         label = label.append(format!("\n{:?}", tarval));
//!     }
//!
//!     if node == cur {
//!         label = label
//!             .style(Style::Filled)
//!             .fillcolor(X11Color::Blue)
//!             .fontcolor(X11Color::White);
//!     }
//!
//!     label
//! });
//! ```
//!
//! # Implementation
//!
//! We lazily start a webserver serving the debugger the first time
//! a breakpoint is encountered. We pause execution by blocking
//! on a message send on a rendevouz-channel to the webserver. The
//! message content is the current compiler state.

pub mod dot;

// Fix legacy imports
pub(crate) use firm_construction as firm;

#[macro_use]
extern crate derive_more;

use crate::dot::GraphState;
use rocket::{
    fairing::{Fairing, Info, Kind},
    get,
    http::{ContentType, Header, Method, Status},
    Request, Response, State,
};
use rocket_contrib::{json::Json, serve::StaticFiles};
use serde_derive::Serialize;
use std::{
    collections::{HashMap, HashSet},
    io::Cursor,
    sync::{
        mpsc::{self, Sender, SyncSender},
        RwLock,
    },
};

#[cfg(feature = "debugger_gui")]
use {std::sync::mpsc::Receiver, std::sync::Mutex, std::thread, std::thread::JoinHandle};

#[cfg(feature = "debugger_gui")]
#[macro_export]
macro_rules! breakpoint {
    ($label:expr, $prog:expr) => {{
        use ::debugging::dot::GraphData;
        crate::debugging::pause(
            crate::debugging::Breakpoint {
                label: $label.to_string(),
                line: line!(),
                column: column!(),
                file: file!(),
            },
            $prog.graph_data(&crate::dot::default_label),
        );
    }};
    ($label:expr, $prog:expr, $labels:expr) => {{
        use ::debugging::dot::GraphData;
        crate::debugging::pause(
            crate::debugging::Breakpoint {
                label: $label.to_string(),
                line: line!(),
                column: column!(),
                file: file!(),
            },
            $prog.graph_data($labels),
        );
    }};
}

#[cfg(not(feature = "debugger_gui"))]
#[macro_export]
macro_rules! breakpoint {
    ($label:expr, $prog:expr) => {{
        let _ = &$label;
        let _ = &$prog;
    }};
    ($label:expr, $prog:expr, $labels:expr) => {{
        // TODO: there has to be a better way
        let _ = &$label;
        let _ = &$prog;
        let _ = &$labels;
    }};
}

#[cfg(feature = "debugger_gui")]
lazy_static::lazy_static! {
    static ref GUI: Mutex<Option<GuiThread>> = Mutex::new(None);
    static ref FILTERS: Mutex<BreakpointFilters> = Mutex::new(BreakpointFilters::default());
}

#[cfg(feature = "debugger_gui")]
fn gui_thread() -> &'static GUI {
    if (*GUI.lock().unwrap()).is_none() {
        spawn_gui_thread();
    }

    &GUI
}

#[cfg(feature = "debugger_gui")]
fn spawn_gui_thread() {
    let (sender, receiver) = mpsc::sync_channel::<MsgToCompiler>(256);

    let _handle = thread::spawn(|| {
        http_server(sender);
    });

    *GUI.lock().unwrap() = Some(GuiThread { _handle, receiver });
}

#[cfg(not(feature = "debugger_gui"))]
#[allow(clippy::implicit_hasher)]
pub fn pause(_breakpoint: Breakpoint, _program: HashMap<String, GraphState>) {}

#[cfg(feature = "debugger_gui")]
#[allow(clippy::implicit_hasher)]
pub fn pause(breakpoint: Breakpoint, program: HashMap<String, GraphState>) {
    if std::env::var("COMPRAKT_DEBUGGER_GUI_DUMP_LIR_DOT_GRAPH_MJ_MAIN_TO_STDOUT").is_ok() {
        if breakpoint.label.matches("LIR").count() > 0 {
            println!("{}", program["mj_main"].dot_content);
        }
    }

    let mut filters = FILTERS.lock().unwrap();

    if filters.is_disabled(&breakpoint, &program) {
        log::info!(
            "ignoring disabled breakpoint {} ({}:{})",
            breakpoint.label,
            breakpoint.file,
            breakpoint.line,
        );

        return;
    }

    let gui = gui_thread().lock().unwrap();

    log::warn!(
        "[waiting at breakpoint] {} ({}:{})",
        breakpoint.label,
        breakpoint.file,
        breakpoint.line,
    );

    let state = CompiliationState::new(breakpoint, program);
    let mut already_sent = false;

    loop {
        let msg = gui
            .as_ref()
            .unwrap()
            .receiver
            .recv()
            .expect("failed to interact with debugger webserver");

        // TODO: the control flow here is suboptimal. If the compiler is not
        // at a breakpoint, the webserver will block on the GetCompilationState
        // and AddFilter indefinitely until the request from the gui times out.
        //
        // We currently get away with this as the compiler is very fast and
        // virtually always at a breakpoint.
        match msg {
            MsgToCompiler::Continue => break,
            MsgToCompiler::GetCompilationState { sender } => {
                sender
                    .send(MsgToGui::CompiliationState {
                        state: state.clone(),
                        already_sent,
                    })
                    .unwrap();
                already_sent = true;
            }
            MsgToCompiler::AddFilter { filter } => {
                filters.add(filter);
            }
        }
    }
}

struct BreakpointFilters {
    filters: HashSet<Filter>,
}

impl BreakpointFilters {
    pub fn new() -> Self {
        Self {
            filters: HashSet::new(),
        }
    }

    pub fn add(&mut self, filter: Filter) -> &mut Self {
        self.filters.insert(filter);
        self
    }

    pub fn is_disabled(
        &self,
        breakpoint: &Breakpoint,
        program: &HashMap<String, GraphState>,
    ) -> bool {
        !self
            .filters
            .iter()
            .any(|filter| filter.matches(breakpoint, program))
    }
}

impl Default for BreakpointFilters {
    fn default() -> Self {
        let mut filters = BreakpointFilters::new();

        if let Ok(labels) = std::env::var("FILTER_BREAKPOINT_LABEL") {
            for label in labels.split(',') {
                if label.trim() == "" {
                    continue;
                }
                filters.add(Filter::breakpoint_label(label.trim()));
            }
        }

        if let Ok(locations) = std::env::var("FILTER_BREAKPOINT_LOCATION") {
            for location in locations.split(',') {
                if location.trim() == "" {
                    continue;
                }

                let parts = location.split(':').collect::<Vec<_>>();

                if parts.len() != 2 {
                    panic!(
                        "'FILTER_BREAKPOINT_LOCATION' env variable has to be \
                         in the format 'file:line'"
                    );
                }

                filters.add(Filter::Location {
                    file: parts[0].trim().to_owned(),
                    line: parts[1]
                        .trim()
                        .parse::<u32>()
                        .expect("'FILTER_BREAKPOINT_LOCATION' contains invalid line number"),
                });
            }
        }

        if let Ok(names) = std::env::var("FILTER_GRAPH_NAME") {
            for name in names.split(',') {
                if name.trim() == "" {
                    continue;
                }
                filters.add(Filter::graph(name.trim()));
            }
        } else {
            filters
                .add(Filter::graph("$WRAPPER$_mjrt_system_in_read"))
                .add(Filter::graph("$WRAPPER$_mjrt_system_out_write"))
                .add(Filter::graph("$WRAPPER$_mjrt_system_out_println"))
                .add(Filter::graph("$WRAPPER$_mjrt_system_out_flush"));
        }

        filters
    }
}

#[derive(PartialEq, Eq, Clone, Debug, Hash)]
enum Filter {
    Location { file: String, line: u32 },
    Label { name: String },
    Graph { name: String },
}

impl Filter {
    fn graph(name: &str) -> Self {
        Filter::Graph {
            name: name.to_owned(),
        }
    }

    fn breakpoint_label(name: &str) -> Self {
        Filter::Label {
            name: name.to_owned(),
        }
    }

    fn matches(&self, breakpoint: &Breakpoint, program: &HashMap<String, GraphState>) -> bool {
        match self {
            Filter::Location { file, line } => breakpoint.line == *line && breakpoint.file == *file,
            Filter::Graph { name } => program
                .values()
                .any(|graph| graph.name.matches(name).count() > 0),
            Filter::Label { name } => breakpoint.label.matches(name).count() > 0,
        }
    }
}

enum MsgToCompiler {
    /// Stop Waiting at the breakpoint and continue
    /// compilation
    Continue,
    ///
    GetCompilationState {
        sender: Sender<MsgToGui>,
    },
    AddFilter {
        filter: Filter,
    },
}

enum MsgToGui {
    CompiliationState {
        state: CompiliationState,
        already_sent: bool,
    },
}

struct GuiThread {
    #[cfg(feature = "debugger_gui")]
    _handle: JoinHandle<()>,
    #[cfg(feature = "debugger_gui")]
    receiver: Receiver<MsgToCompiler>,
}

#[derive(Debug, Clone, Serialize)]
pub struct Breakpoint {
    pub label: String,
    pub file: &'static str,
    pub line: u32,   // 1-based
    pub column: u32, // 1-based
}

#[derive(Debug, Clone, Serialize)]
struct CompiliationState {
    breakpoint: Breakpoint,
    // maps function name to dot file
    graphs: HashMap<String, GraphState>,
}

impl CompiliationState {
    #[cfg(feature = "debugger_gui")]
    fn new(breakpoint: Breakpoint, graphs: HashMap<String, GraphState>) -> Self {
        Self { breakpoint, graphs }
    }
}

struct Debugger {
    breakpoints: RwLock<Vec<CompiliationState>>,
    sender: SyncSender<MsgToCompiler>,
}

impl Debugger {
    fn new(sender: SyncSender<MsgToCompiler>) -> Self {
        Self {
            breakpoints: RwLock::new(Vec::new()),
            sender,
        }
    }
}

struct DebuggerState(Debugger);

pub struct CORS();

impl Fairing for CORS {
    fn info(&self) -> Info {
        Info {
            name: "Add CORS headers to requests",
            kind: Kind::Response,
        }
    }

    fn on_response(&self, request: &'_ Request<'_>, response: &'_ mut Response<'_>) {
        response.set_header(Header::new("Access-Control-Allow-Origin", "*"));
        response.set_header(Header::new(
            "Access-Control-Allow-Methods",
            "POST, GET, OPTIONS",
        ));
        response.set_header(Header::new("Access-Control-Allow-Headers", "Content-Type"));

        if request.method() == Method::Options {
            response.set_header(ContentType::Plain);
            response.set_sized_body(Cursor::new(""));
        }
    }
}

#[get("/breakpoint/continue")]
fn breakpoint_continue(debugger: State<'_, DebuggerState>) -> Result<(), Status> {
    debugger.0.sender.send(MsgToCompiler::Continue).unwrap();
    Ok(())
}

fn add_filter(debugger: &State<'_, DebuggerState>, filter: Filter) {
    debugger
        .0
        .sender
        .send(MsgToCompiler::AddFilter { filter })
        .unwrap();
}

fn check_updates(debugger: &State<'_, DebuggerState>) {
    let (sender, receiver) = mpsc::channel();
    debugger
        .0
        .sender
        .send(MsgToCompiler::GetCompilationState { sender })
        .unwrap();

    match receiver.recv().unwrap() {
        MsgToGui::CompiliationState {
            state,
            already_sent,
        } => {
            if !already_sent {
                debugger.0.breakpoints.write().unwrap().push(state);
            }
        }
    };
}

#[get("/snapshot/<index>")]
fn snapshot_at_index(
    index: usize,
    debugger: State<'_, DebuggerState>,
) -> Result<Json<Option<CompiliationState>>, Status> {
    check_updates(&debugger);
    Ok(Json(
        debugger.0.breakpoints.read().unwrap().get(index).cloned(),
    ))
}

#[get("/breakpoint/all")]
fn breakpoint_list(debugger: State<'_, DebuggerState>) -> Result<Json<Vec<Breakpoint>>, Status> {
    check_updates(&debugger);
    Ok(Json(
        debugger
            .0
            .breakpoints
            .read()
            .unwrap()
            .iter()
            .map(|v| v.breakpoint.clone())
            .collect(),
    ))
}

#[get("/breakpoint/filter/add/graph/<name>")]
fn breakpoint_filter_name(debugger: State<'_, DebuggerState>, name: String) -> Result<(), Status> {
    add_filter(&debugger, Filter::graph(&name));
    Ok(())
}

#[get("/breakpoint/filter/add/label/<name>")]
fn breakpoint_filter_label(debugger: State<'_, DebuggerState>, name: String) -> Result<(), Status> {
    add_filter(&debugger, Filter::breakpoint_label(&name));
    Ok(())
}

#[get("/breakpoint/filter/add/location/<file>/<line>")]
fn breakpoint_filter_location(
    debugger: State<'_, DebuggerState>,
    file: String,
    line: u32,
) -> Result<(), Status> {
    add_filter(&debugger, Filter::Location { file, line });
    Ok(())
}

fn http_server(sender: SyncSender<MsgToCompiler>) {
    // TODO: compile static files into binary
    let static_files = format!(
        "{}/debugger-gui/dist/development/",
        env!("CARGO_MANIFEST_DIR")
    );

    log::debug!("static files served from {}", static_files);

    rocket::ignite()
        .attach(CORS())
        .mount("/", StaticFiles::from(&static_files))
        .mount(
            "/",
            rocket::routes![
                breakpoint_continue,
                breakpoint_list,
                snapshot_at_index,
                breakpoint_filter_location,
                breakpoint_filter_label,
                breakpoint_filter_name
            ],
        )
        .manage(DebuggerState(Debugger::new(sender)))
        .launch();
}
