use libfirm_rs_bindings::*;

use std::ffi::CStr;

fn main() {
    unsafe {
        build_running_sum();
    }
}

macro_rules! cstr {
    ($e:expr) => {
        CStr::from_bytes_with_nul($e.as_bytes()).unwrap().as_ptr()
    };
}

unsafe fn build_running_sum() {
    ir_init_library();

    let triple = ir_get_host_machine_triple();
    ir_target_set_triple(triple);
    ir_target_option(cstr!("pic=1\0"));

    ir_target_option(cstr!("dump=all\0"));
    ir_target_init();
    set_optimize(0);

    let int_type: *mut ir_type = new_type_primitive(mode_Is);
    let unsigned_type: *mut ir_type = new_type_primitive(mode_Iu);
    let int_array_type: *mut ir_type = new_type_array(int_type, 0);

	let function_type: *mut ir_type = new_type_method(3, 1, false.into(), cc_cdecl_set, mtp_additional_properties::NoProperty);
	set_method_param_type(function_type, 0, new_type_pointer(int_type));
	set_method_param_type(function_type, 1, new_type_pointer(int_type));
	set_method_param_type(function_type, 2, unsigned_type);
	set_method_res_type(function_type, 0, int_type);

    let global_type: *mut ir_type = get_glob_type();
    let name: *mut ident = new_id_from_str(cstr!("running_sum\0"));
    let our_function: *mut ir_entity  = new_entity(global_type, name, function_type);

	let n_slots = 5; // values, output, length, total, i
	let graph: *mut ir_graph = new_ir_graph(our_function, n_slots);
	set_current_ir_graph(graph);


	// If you want to have a look at the nodes which are not yet
	// used by any others, try calling "keep_alive" on them.
	// This is only for debugging, as it might break later phases.
	// We're luck here, though.
	keep_alive(get_irg_start_block(graph));

	dump_ir_graph(graph, cstr!("empty\0"));

	/* start block */
	let start_block: *mut ir_node  = get_irg_start_block(graph);
	set_cur_block(start_block);

	set_value(0, new_Proj(get_irg_args(graph), mode_P, 0));
	set_value(1, new_Proj(get_irg_args(graph), mode_P, 1));
	set_value(2, new_Proj(get_irg_args(graph), mode_Iu, 2));

	/* int total = 0; */
	let int0: *mut ir_node = new_Const(new_tarval_from_long(0, mode_Is));
	set_value(3, int0);
	/* unsigned i = 0; */
	let unsigned0: *mut ir_node = new_Const(new_tarval_from_long(0, mode_Iu));
	set_value(4, unsigned0);

	let start_jmp = new_Jmp();

	/* loop header */
	let loop_header = new_immBlock();
	add_immBlock_pred(loop_header, start_jmp);
	set_cur_block(loop_header);

	/* if (i < length) */
	let lh_i = get_value(4, mode_Iu);
	let lh_length = get_value(2, mode_Iu);
	let lh_cmp = new_Cmp(lh_i, lh_length, ir_relation::Less);

	let lh_cond = new_Cond(lh_cmp);
	let lh_true = new_Proj(lh_cond, mode_X, pn_Cond::True);
	let lh_false = new_Proj(lh_cond, mode_X, pn_Cond::False);

	/* loop body */
	let loop_body = new_immBlock();
	add_immBlock_pred(loop_body, lh_true);
	set_cur_block(loop_body);

    /* total += values[i] */
	let lb_values = get_value(0, mode_P);
	let mut lb_i = get_value(4, mode_Iu);
	let lb_values_i_ptr = new_Sel(lb_values, lb_i, int_array_type);
	let mut mem = get_store();
	let lb_load = new_Load(mem, lb_values_i_ptr, mode_Is, int_type, ir_cons_flags::None);
	set_store(new_Proj(lb_load, mode_M, pn_Load::M));

	let mut lb_total = get_value(3, mode_Is);
	set_value(3, new_Add(lb_total, new_Proj(lb_load, mode_Is, pn_Load::Res)));

	/* output[i] = total; */
	lb_total = get_value(3, mode_Is);
	let lb_output = get_value(1, mode_P);
	let lb_output_i_ptr = new_Sel(lb_output, lb_i, int_array_type);
	mem = get_store();
	let lb_store = new_Store(mem, lb_values_i_ptr, lb_total, int_type, ir_cons_flags::None);
	set_store(new_Proj(lb_store, mode_M, pn_Store::M));

	/* i++; */
	lb_i = get_value(4, mode_Iu);
	set_value(4, new_Add(lb_i, new_Const(new_tarval_from_long(1, mode_Iu))));

	let lb_jmp = new_Jmp();

	/* goto loop header */
	add_immBlock_pred(loop_header, lb_jmp);

	/* return block */
	let return_block = new_immBlock();
	add_immBlock_pred(return_block, lh_false);
	set_cur_block(return_block);

	let rb_total: *mut ir_node = get_value(3, mode_Is);
	mem = get_store();
    let additional_inputs = &[rb_total];
	let rb_return = new_Return(mem, additional_inputs.len() as i32, additional_inputs as *const *mut ir_node);

	add_immBlock_pred(get_irg_end_block(graph), rb_return);

	keep_alive(lb_store);

	dump_ir_graph(graph, cstr!("immature\0"));

	mature_immBlock(start_block);
	mature_immBlock(loop_header);
	mature_immBlock(loop_body);
	mature_immBlock(return_block);
	irg_finalize_cons(graph);

	dump_ir_graph(graph, cstr!("ssa-constructed\0"));

	lower_highlevel_graph(graph);
	be_lower_for_target();

	dump_ir_graph(graph, cstr!("lowered\0"));

	be_main(stdout, cstr!("<stdin>\0"));

	ir_finish();


}
