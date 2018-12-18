//! This is a browser based graphical debugger for libfirm graphs.
//! Simply drop
//!
//! # Usage
//!
//! Use the `breakpoint!` macro to interact with the graphical debugger.
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
//! // assume a context similar to:
//! // let values :HashMap<Node,Tarval> = HashMap::new();
//! // while let Some(cur) = next_node() 
//! breakpoint!("Constant Folding: iteration", graph, &|node| {
//!     let mut label = default_label(node); // prints node id and node kind
//!     if let Some(tarval) = values.get(&node) {
//!         label.append(format!("\n{:?}", tarval));
//!     }
//!
//!     if node == cur {
//!         label.dotted(true);
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

use rocket_contrib::serve::StaticFiles;
use serde_derive::{Serialize};
use rocket_contrib::json::Json;
use rocket::response::content;
use rocket::response::status;
use rocket::http::Status;
use rocket::State;
use rocket::get;
use std::{sync::{RwLock, Mutex}, thread::{self, JoinHandle}};
use std::sync::mpsc::TryRecvError;
use std::sync::mpsc::{self, Sender, SyncSender, Receiver};
use std::collections::hash_map::HashMap;
use crate::firm::Program;
use libfirm_rs::graph::Graph;
use libfirm_rs::entity::Entity;
use crate::dot::{GraphState, GraphData};

lazy_static::lazy_static! {
    static ref GUI: Mutex<Option<GuiThread>> = Mutex::new(None);
}

#[cfg(feature = "gui_debugger")]
#[macro_export]
macro_rules! breakpoint {
    ($label:expr, $prog:expr) => {{
        use crate::dot::GraphData;
        crate::debugging::pause(crate::debugging::Breakpoint {
            label: $label.to_string(),
            line: line!(),
            column: column!(),
            file: file!()
        }, $prog.graph_data(&crate::dot::default_label)); }};
    ($label:expr, $prog:expr, $labels:expr) => {{
        use crate::dot::GraphData;
        crate::debugging::pause(crate::debugging::Breakpoint {
            label: $label.to_string(),
            line: line!(),
            column: column!(),
            file: file!()
        }, $prog.graph_data($labels)); }};
}

#[cfg(not(feature = "gui_debugger"))]
#[macro_export]
macro_rules! breakpoint {
    ($label:expr, $prog:expr) => {{ }};
    ($label:expr, $prog:expr, $labels:expr) => {{ }};
}

fn gui_thread() -> &'static GUI {
    if (*GUI.lock().unwrap()).is_none() {
        spawn_gui_thread();
    }

    &GUI
}

fn spawn_gui_thread() {
    let (sender, receiver) = mpsc::sync_channel::<MsgToCompiler>(256);

    let handle = thread::spawn(|| {
        http_server(sender);
    });

    *GUI.lock().unwrap() = Some(GuiThread {
        handle,
        receiver,
    });
}

#[cfg(not(feature = "gui_debugger"))]
pub fn pause(_breakpoint: Breakpoint, _program :HashMap<String, GraphState>) {
}

#[cfg(feature = "gui_debugger")]
pub fn pause(breakpoint: Breakpoint, program :HashMap<String, GraphState>) {
    log::debug!("waiting at breakpoint: {:?}", breakpoint);
    let state = CompiliationState::new(breakpoint, program);
    let gui = gui_thread().lock().unwrap();
    let mut already_sent = false;

    loop {
        let msg = gui.as_ref().unwrap().receiver.recv().expect("failed to interact with debugger webserver");

        match msg {
            MsgToCompiler::Continue => break,
            MsgToCompiler::GetCompilationState { sender } => {
                sender.send(MsgToGui::CompiliationState{
                    state: state.clone(),
                    already_sent
                });
                already_sent = true;
            },
        }
    }
}

enum MsgToCompiler {
    /// Stop Waiting at the breakpoint and continue
    /// compilation
    Continue,
    /// 
    GetCompilationState {sender: Sender<MsgToGui>}
    //DisableBreakpoint,
}

enum MsgToGui {
    CompiliationState {
        state: CompiliationState,
        already_sent: bool
    }
}

struct GuiThread {
    handle: JoinHandle<()>,
    receiver: Receiver<MsgToCompiler>,
}

#[derive(Debug,Clone,Serialize)]
pub struct Breakpoint {
    pub label: String,
    pub file: &'static str,
    pub line: u32,   // 1-based
    pub column: u32, // 1-based
}

#[derive(Debug,Clone,Serialize)]
struct CompiliationState {
    breakpoint: Breakpoint,
    // maps function name to dot file
    dot_files: HashMap<String, GraphState>
}

impl CompiliationState {
    fn new(breakpoint: Breakpoint, program : HashMap<String, GraphState>) -> Self {
        
        Self {
            breakpoint,
            dot_files: program
        }
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

// TODO serving this via GET is not standard conform, but convenient during development
#[get("/breakpoint/continue")]
fn breakpoint_continue(debugger: State<'_, DebuggerState>) -> Result<(), Status> {
    debugger.0.sender.send(MsgToCompiler::Continue).unwrap();
    Ok(())
}

fn check_updates(debugger: &State<'_, DebuggerState>) {
    let (sender, receiver) = mpsc::channel();
    debugger.0.sender.send(MsgToCompiler::GetCompilationState{sender}).unwrap();

    match receiver.recv().unwrap() {
        MsgToGui::CompiliationState{state,already_sent} => {
            if !already_sent {
                debugger.0.breakpoints.write().unwrap().push(state);
            }
        }
    };
}

#[get("/snapshot/latest")]
fn breakpoint(debugger: State<'_, DebuggerState>) -> Result<Json<Option<CompiliationState>>, Status> {
    // we have a http server --> compiler channel, build a compiler --> http server channel that
    // can be used for anwsering
    check_updates(&debugger);
    Ok(Json(debugger.0.breakpoints.read().unwrap().last().map(|v| v.clone())))
}

#[get("/snapshot/<index>")]
fn snapshot_at_index(index: usize, debugger: State<'_, DebuggerState>) -> Result<Json<Option<CompiliationState>>, Status> {
    // we have a http server --> compiler channel, build a compiler --> http server channel that
    // can be used for anwsering
    check_updates(&debugger);
    Ok(Json(debugger.0.breakpoints.read().unwrap().get(index).map(|v| v.clone())))
}

#[get("/breakpoint/all")]
fn breakpoint_list(debugger: State<'_, DebuggerState>) -> Result<Json<Vec<Breakpoint>>, Status> {
    check_updates(&debugger);
    Ok(Json(debugger.0.breakpoints.read().unwrap().iter().map(|v| {
        v.breakpoint.clone()
    }).collect()))
}

fn http_server(sender :SyncSender<MsgToCompiler>) {
    // TODO: compile static files into binary
    let static_files = format!("{}/debugger-gui/dist/development/", env!("CARGO_MANIFEST_DIR"));

    log::debug!("static files served from {}", static_files);

    rocket::ignite()
        .mount("/", StaticFiles::from(&static_files))
        .mount("/", rocket::routes![breakpoint_continue, breakpoint, breakpoint_list, snapshot_at_index])
        .manage(DebuggerState(Debugger::new(sender)))
        .launch();
}
