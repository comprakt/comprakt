use crate::AssembleOptions;

use libfirm_rs_bindings::*;
use std::ffi::CStr;

///
/// ```c
/// // This is the function we want to build in Firm
/// int running_sum(int *values, int *output, unsigned length)
/// {
/// 	int total = 0;
///
/// 	for (unsigned i = 0; i < length; i++) {
/// 		total += values[i];
/// 		output[i] = total;
/// 	}
///
/// 	return total;
/// }
/// ```
pub unsafe fn print_machine_triple(triple: *mut ir_machine_triple_t) {
    let cpu = ir_triple_get_cpu_type(triple);
    let manu = ir_triple_get_manufacturer(triple);
    let os = ir_triple_get_operating_system(triple);

    println!("TARGET TRIPLE");
    println!("=============\n");

    println!(
        "CPU:              \t{}",
        CStr::from_ptr(cpu).to_string_lossy()
    );
    println!(
        "Manufacturer:     \t{}",
        CStr::from_ptr(manu).to_string_lossy()
    );
    println!(
        "Operating System: \t{}",
        CStr::from_ptr(os).to_string_lossy()
    );

    let pointer_size = ir_target_pointer_size();
    println!("Pointer Size:     \t{} Byte", pointer_size);
}

pub unsafe fn assemble(opts: &AssembleOptions) {
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
    ir_target_option(CStr::from_bytes_with_nul(b"dump=all\0").unwrap().as_ptr());

    ir_target_init();

    set_optimize(0);

    let int_type = new_type_primitive(mode_Is);
    let unsigned_type = new_type_primitive(mode_Iu);
    let int_array_type = new_type_array(int_type, 0);

    let function_type = new_type_method(
        3,
        1,
        0,
        cc_cdecl_set,
        mtp_additional_properties_mtp_no_property,
    );

    set_method_param_type(function_type, 0, new_type_pointer(int_type));
    set_method_param_type(function_type, 1, new_type_pointer(int_type));
    set_method_param_type(function_type, 2, unsigned_type);
    set_method_res_type(function_type, 0, int_type);

    let global_type = get_glob_type();
    let name = new_id_from_str(
        CStr::from_bytes_with_nul(b"running_sum\0")
            .unwrap()
            .as_ptr(),
    );
    let our_function = new_entity(global_type, name, function_type);

    let n_slots = 5; // values, output, length, total, i
    let graph = new_ir_graph(our_function, n_slots);
    set_current_ir_graph(graph);

    //// If you want to have a look at the nodes which are not yet
    //// used by any others, try calling "keep_alive" on them.
    //// This is only for debugging, as it might break later phases.
    //// We're luck here, though.
    //keep_alive(get_irg_start_block(graph));

    //dump_ir_graph(graph, "empty");

    let start_block = get_irg_start_block(graph);
    set_cur_block(start_block);

    set_value(0, new_Proj(get_irg_args(graph), mode_P, 0));
    set_value(1, new_Proj(get_irg_args(graph), mode_P, 1));
    set_value(2, new_Proj(get_irg_args(graph), mode_Iu, 2));

    //[> int total = 0; <]
    let int0 = new_Const(new_tarval_from_long(0, mode_Is));
    set_value(3, int0);
    //[> unsigned i = 0; <]
    let unsigned0 = new_Const(new_tarval_from_long(0, mode_Iu));
    set_value(4, unsigned0);

    let start_jmp = new_Jmp();

    //[> loop header <]
    let loop_header = new_immBlock();
    add_immBlock_pred(loop_header, start_jmp);
    set_cur_block(loop_header);

    //[> if (i < length) <]
    let lh_i = get_value(4, mode_Iu);
    let lh_length = get_value(2, mode_Iu);
    let lh_cmp = new_Cmp(lh_i, lh_length, ir_relation_ir_relation_less);

    let lh_cond = new_Cond(lh_cmp);
    let lh_true = new_Proj(lh_cond, mode_X, pn_Cond_pn_Cond_true);
    let lh_false = new_Proj(lh_cond, mode_X, pn_Cond_pn_Cond_false);

    //[> loop body <]
    let loop_body = new_immBlock();
    add_immBlock_pred(loop_body, lh_true);
    set_cur_block(loop_body);

    //[> total += values[i] <]
    let lb_values = get_value(0, mode_P);
    let mut lb_i = get_value(4, mode_Iu);
    let lb_values_i_ptr = new_Sel(lb_values, lb_i, int_array_type);
    let mut mem = get_store();
    let lb_load = new_Load(
        mem,
        lb_values_i_ptr,
        mode_Is,
        int_type,
        ir_cons_flags_cons_none,
    );
    set_store(new_Proj(lb_load, mode_M, pn_Load_pn_Load_M));

    let mut lb_total = get_value(3, mode_Is);
    set_value(
        3,
        new_Add(lb_total, new_Proj(lb_load, mode_Is, pn_Load_pn_Load_res)),
    );

    //[> output[i] = total; <]
    lb_total = get_value(3, mode_Is);
    let lb_output = get_value(1, mode_P);
    let lb_output_i_ptr = new_Sel(lb_output, lb_i, int_array_type);
    mem = get_store();
    let lb_store = new_Store(
        mem,
        lb_values_i_ptr,
        lb_total,
        int_type,
        ir_cons_flags_cons_none,
    );
    set_store(new_Proj(lb_store, mode_M, pn_Store_pn_Store_M));

    //[> i++; <]
    lb_i = get_value(4, mode_Iu);
    set_value(
        4,
        new_Add(lb_i, new_Const(new_tarval_from_long(1, mode_Iu))),
    );

    let lb_jmp = new_Jmp();

    //[> goto loop header <]
    add_immBlock_pred(loop_header, lb_jmp);

    //[> return block <]
    let return_block = new_immBlock();
    add_immBlock_pred(return_block, lh_false);
    set_cur_block(return_block);

    let rb_total = get_value(3, mode_Is);
    mem = get_store();
    let rb_return = new_Return(mem, 1, &[rb_total] as *const *mut ir_node);

    add_immBlock_pred(get_irg_end_block(graph), rb_return);

    mature_immBlock(start_block);
    mature_immBlock(loop_header);
    mature_immBlock(loop_body);
    mature_immBlock(return_block);
    irg_finalize_cons(graph);

    if let Some(ref path) = opts.dump_firm_graph {
        let mut cpath = path.to_string_lossy().to_string();
        cpath.push('\0');

        let cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
            .unwrap()
            .as_ptr();
        dump_ir_graph(graph, cstr);
    }

    lower_highlevel_graph(graph);
    be_lower_for_target();

    if let Some(ref path) = opts.dump_lowered_firm_graph {
        let mut cpath = path.to_string_lossy().to_string();
        cpath.push('\0');

        let cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
            .unwrap()
            .as_ptr();
        dump_ir_graph(graph, cstr);
    }

    if let Some(ref _path) = opts.dump_assembler {
        //let mut cpath = path.to_string_lossy().to_string();
        //cpath.push('\0');

        //let cstr = CStr::from_bytes_with_nul(cpath.as_bytes())
        //.unwrap()
        //.as_ptr();
        // TODO: open file with C api instead of foring stdout
        be_main(
            stdout,
            CStr::from_bytes_with_nul(b"<stdin>\0").unwrap().as_ptr(),
        );
    }

    ir_finish();
}
