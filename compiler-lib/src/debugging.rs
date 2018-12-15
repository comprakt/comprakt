//! This is a browser based graphical debugger for libfirm graphs.
//! Simply drop
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

lazy_static::lazy_static! {
    static ref GUI: Mutex<Option<GuiThread>> = Mutex::new(None);
}

#[macro_export]
macro_rules! breakpoint {
    ($label:expr, $prog:expr) => {{
        crate::debugging::pause(crate::debugging::Breakpoint {
            label: $label,
            line: line!(),
            column: column!(),
            file: file!()
        }, $prog); }}; }

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

pub fn pause(breakpoint: Breakpoint, program :&Program<'_,'_>) {
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

#[derive(Debug,Clone,Serialize)]
struct GraphState {
    class_name: String,
    method_name: String,
    dot_file: String
}

impl CompiliationState {
    pub fn new(breakpoint: Breakpoint, program :&Program<'_,'_>) -> Self {

        let mut dot_files = HashMap::new();

        for (class_name, class) in &program.classes {
            for (method_name, method) in &class.borrow().methods {
                if let Some(graph) = method.borrow().graph {
                    let graph : Graph = graph.into();
                    let internal_name = Entity::new(method.borrow().entity.into()).name_string();
                    dot_files.insert(internal_name, GraphState {
                        class_name: class_name.to_string(),
                        method_name: method_name.to_string(),
                        dot_file: graph.dot_data()
                    });
                }
            }
        }
        
        Self {
            breakpoint,
            dot_files
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
fn breakpoint_continue(debugger: State<DebuggerState>) -> Result<(), Status> {
    debugger.0.sender.send(MsgToCompiler::Continue).unwrap();
    Ok(())
}

fn check_updates(debugger: &State<DebuggerState>) {
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
fn breakpoint(debugger: State<DebuggerState>) -> Result<Json<Option<CompiliationState>>, Status> {
    // we have a http server --> compiler channel, build a compiler --> http server channel that
    // can be used for anwsering
    check_updates(&debugger);
    Ok(Json(debugger.0.breakpoints.read().unwrap().last().map(|v| v.clone())))
}

#[get("/snapshot/<index>")]
fn snapshot_at_index(index: usize, debugger: State<DebuggerState>) -> Result<Json<Option<CompiliationState>>, Status> {
    // we have a http server --> compiler channel, build a compiler --> http server channel that
    // can be used for anwsering
    check_updates(&debugger);
    Ok(Json(debugger.0.breakpoints.read().unwrap().get(index).map(|v| v.clone())))
}

#[get("/breakpoint/all")]
fn breakpoint_list(debugger: State<DebuggerState>) -> Result<Json<Vec<Breakpoint>>, Status> {
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
