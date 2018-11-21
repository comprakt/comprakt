use libfirm_rs::bindings::*;
use std::{ffi::CStr, path::PathBuf};

#[derive(Debug, Clone)]
pub struct Options {
    pub dump_firm_graph: Option<PathBuf>,
    pub dump_lowered_firm_graph: Option<PathBuf>,
    pub dump_assembler: Option<PathBuf>,
}

pub unsafe fn build(opts: &Options) {
    ir_init_library();

    // this call panics on error
    let triple = ir_get_host_machine_triple();
    ir_target_set_triple(triple);
    //print_machine_triple(triple);

    // pic=1 means 'generate position independent code'
    ir_target_option(CStr::from_bytes_with_nul(b"pic=1\0").unwrap().as_ptr());

    // The backend can also dump graphs after each step.
    // You can ignore these as you will be writing your own back end anyway.
    // Note(Reiner): seems undocumented?!
    //ir_target_option(CStr::from_bytes_with_nul(b"dump=all\0").unwrap().as_ptr());

    ir_target_init();

    set_optimize(0);

    // TODO: build and finalize graph!

    if let Some(ref path) = opts.dump_firm_graph {
        let mut cpath = path.to_string_lossy().to_string();
        cpath.push('\0');

        let _cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
            .unwrap()
            .as_ptr();
        //dump_ir_graph(graph, cstr);
    }

    //lower_highlevel_graph(graph);
    //be_lower_for_target();

    if let Some(ref path) = opts.dump_lowered_firm_graph {
        let mut cpath = path.to_string_lossy().to_string();
        cpath.push('\0');

        let _cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
            .unwrap()
            .as_ptr();
        //dump_ir_graph(graph, cstr);
    }

    if let Some(ref _path) = opts.dump_assembler {
        // TODO: open file with C api instead of foring stdout
        // TODO: print target machine triple, input file name, compiler version
        // on top of assembler output
        be_main(
            stdout,
            CStr::from_bytes_with_nul(b"<stdin>\0").unwrap().as_ptr(),
        );
    }

    ir_finish();
}

pub unsafe fn _print_machine_triple(triple: *mut ir_machine_triple_t) {
    let cpu = ir_triple_get_cpu_type(triple);
    let manu = ir_triple_get_manufacturer(triple);
    let os = ir_triple_get_operating_system(triple);

    println!("% TARGET TRIPLE");
    println!("% =============\n");

    println!(
        "% CPU:                  {}",
        CStr::from_ptr(cpu).to_string_lossy()
    );
    println!(
        "% Manufacturer:         {}",
        CStr::from_ptr(manu).to_string_lossy()
    );
    println!(
        "% Operating System:     {}",
        CStr::from_ptr(os).to_string_lossy()
    );

    let pointer_size = ir_target_pointer_size();
    println!("% Pointer Size:         {} Byte", pointer_size);
}
