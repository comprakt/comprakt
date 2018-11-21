use libfirm_rs_bindings::*;
use libfirm_rs;

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

    let int_type = libfirm_rs::PrimitiveType::isize();
    let unsigned_type = libfirm_rs::PrimitiveType::usize();
    let int_array_type = libfirm_rs::ArrayType::varlength(int_type);

    let function_type = libfirm_rs::FunctionType::new()
        .param(int_array_type.pointer())
        .param(int_array_type.pointer())
        .param(unsigned_type)
        .res(int_type)
        .build();

	//let function_type: *mut ir_type = new_type_method(3, 1, false.into(), cc_cdecl_set, mtp_additional_properties::NoProperty);
	//set_method_param_type(function_type, 0, new_type_pointer(int_type));
	//set_method_param_type(function_type, 1, new_type_pointer(int_type));
	//set_method_param_type(function_type, 2, unsigned_type);
	//set_method_res_type(function_type, 0, int_type);


    let graph = libfirm_rs::Graph::function("running_sum".to_owned(), function_type, 5);
	set_current_ir_graph(graph.into());


	// If you want to have a look at the nodes which are not yet
	// used by any others, try calling "keep_alive" on them.
	// This is only for debugging, as it might break later phases.
	// We're luck here, though.
	keep_alive(graph.start_block().into());

	dump_ir_graph(graph.into(), cstr!("empty\0"));

	/* start block */
	let start_block  = graph.start_block();
	set_cur_block(start_block.into());

	set_value(0, new_Proj(get_irg_args(graph.into()), mode::P, 0));
	set_value(1, new_Proj(get_irg_args(graph.into()), mode::P, 1));
	set_value(2, new_Proj(get_irg_args(graph.into()), mode::Iu, 2));

	/* int total = 0; */
	let int0: *mut ir_node = new_Const(new_tarval_from_long(0, mode::Is));
	set_value(3, int0);
	/* unsigned i = 0; */
	let unsigned0: *mut ir_node = new_Const(new_tarval_from_long(0, mode::Iu));
	set_value(4, unsigned0);

	let start_jmp = new_Jmp();

	/* loop header */
	let loop_header = new_immBlock();
	add_immBlock_pred(loop_header, start_jmp);
	set_cur_block(loop_header);

	/* if (i < length) */
	let lh_i = get_value(4, mode::Iu);
	let lh_length = get_value(2, mode::Iu);
	let lh_cmp = new_Cmp(lh_i, lh_length, ir_relation::Less);

	let lh_cond = new_Cond(lh_cmp);
	let lh_true = new_Proj(lh_cond, mode::X, pn_Cond::True);
	let lh_false = new_Proj(lh_cond, mode::X, pn_Cond::False);

	/* loop body */
	let loop_body = new_immBlock();
	add_immBlock_pred(loop_body, lh_true);
	set_cur_block(loop_body);

    /* total += values[i] */
	let lb_values = get_value(0, mode::P);
	let mut lb_i = get_value(4, mode::Iu);
	let lb_values_i_ptr = new_Sel(lb_values, lb_i, int_array_type.into());
	let mut mem = get_store();
	let lb_load = new_Load(mem, lb_values_i_ptr, mode::Is, int_type.into(), ir_cons_flags::None);
	set_store(new_Proj(lb_load, mode::M, pn_Load::M));

	let mut lb_total = get_value(3, mode::Is);
	set_value(3, new_Add(lb_total, new_Proj(lb_load, mode::Is, pn_Load::Res)));

	/* output[i] = total; */
	lb_total = get_value(3, mode::Is);
	let lb_output = get_value(1, mode::P);
	let lb_output_i_ptr = new_Sel(lb_output, lb_i, int_array_type.into());
	mem = get_store();
	let lb_store = new_Store(mem, lb_values_i_ptr, lb_total, int_type.into(), ir_cons_flags::None);
	set_store(new_Proj(lb_store, mode::M, pn_Store::M));

	/* i++; */
	lb_i = get_value(4, mode::Iu);
	set_value(4, new_Add(lb_i, new_Const(new_tarval_from_long(1, mode::Iu))));

	let lb_jmp = new_Jmp();

	/* goto loop header */
	add_immBlock_pred(loop_header, lb_jmp);

	/* return block */
	let return_block = new_immBlock();
	add_immBlock_pred(return_block, lh_false);
	set_cur_block(return_block);

	let rb_total: *mut ir_node = get_value(3, mode::Is);
	mem = get_store();
    let additional_inputs = &[rb_total];
	let rb_return = new_Return(mem, additional_inputs.len() as i32, additional_inputs as *const *mut ir_node);

	add_immBlock_pred(get_irg_end_block(graph.into()), rb_return);

	keep_alive(lb_store);

	dump_ir_graph(graph.into(), cstr!("immature\0"));

	mature_immBlock(start_block.into());
	mature_immBlock(loop_header);
	mature_immBlock(loop_body);
	mature_immBlock(return_block);
	irg_finalize_cons(graph.into());

	dump_ir_graph(graph.into(), cstr!("ssa-constructed\0"));

	lower_highlevel_graph(graph.into());
	be_lower_for_target();

	dump_ir_graph(graph.into(), cstr!("lowered\0"));

	be_main(stdout, cstr!("<stdin>\0"));

	ir_finish();


}
