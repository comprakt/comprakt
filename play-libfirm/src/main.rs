use libfirm_rs_bindings::*;
use libfirm_rs::*;

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
//	set_current_ir_graph(graph.into());


	// If you want to have a look at the nodes which are not yet
	// used by any others, try calling "keep_alive" on them.
	// This is only for debugging, as it might break later phases.
	// We're luck here, though.
	keep_alive(graph.start_block().into());

	dump_ir_graph(graph.into(), cstr!("empty\0"));

	/* start block */
	let start_block  = graph.start_block();
	set_r_cur_block(graph.into(), start_block.into());

    let args= graph.args_node();
    graph.set_value(0, args.project(mode::P, 0));
	graph.set_value(1, args.project(mode::P, 1));
	graph.set_value(2, args.project(mode::Iu, 2));

	/* int total = 0; */
	let int0 = graph.new_const(new_tarval_from_long(0, mode::Is));
	graph.set_value(3, int0);
	/* unsigned i = 0; */
	let unsigned0 = graph.new_const(new_tarval_from_long(0, mode::Iu));
	graph.set_value(4, unsigned0);

	let start_jmp = start_block.new_jmp();

	/* loop header */
	let loop_header = graph.new_imm_block(start_jmp);
	set_r_cur_block(graph.into(), loop_header.into());

	/* if (i < length) */
	let lh_i = graph.get_value(4, mode::Iu);
	let lh_length = graph.get_value(2, mode::Iu);
	let lh_cmp = loop_header.new_cmp(lh_i, lh_length, ir_relation::Less);

	let lh_cond = loop_header.new_cond(lh_cmp);
	let lh_true = lh_cond.project_true();
	let lh_false = lh_cond.project_false();

	/* loop body */
	let loop_body = graph.new_imm_block(lh_true);
	set_r_cur_block(graph.into(), loop_body.into());

    /* total += values[i] */
	let lb_values = graph.get_value(0, mode::P);
	let mut lb_i = graph.get_value(4, mode::Iu);
	let lb_values_i_ptr = loop_body.new_sel(lb_values, lb_i, int_array_type.into());
	let mut mem = get_r_store(graph.into());
	let lb_load = new_r_Load(loop_body.into(), mem, lb_values_i_ptr.into(), mode::Is, int_type.into(), ir_cons_flags::None);
	set_r_store(graph.into(), new_r_Proj(lb_load, mode::M, pn_Load::M));

	let mut lb_total = graph.get_value(3, mode::Is);
	graph.set_value(3, loop_body.new_add(lb_total, new_r_Proj(lb_load, mode::Is, pn_Load::Res)));

	/* output[i] = total; */
	lb_total = graph.get_value(3, mode::Is);
	let lb_output = graph.get_value(1, mode::P);
	let lb_output_i_ptr = new_r_Sel(loop_body.into(), lb_output, lb_i, int_array_type.into());
	mem = get_r_store(graph.into());
	let lb_store = new_r_Store(loop_body.into(), mem, lb_values_i_ptr.into(), lb_total, int_type.into(), ir_cons_flags::None);
	set_r_store(graph.into(), new_r_Proj(lb_store, mode::M, pn_Store::M));

	/* i++; */
	lb_i = graph.get_value(4, mode::Iu);
	graph.set_value(4, loop_body.new_add(lb_i, graph.new_const(new_tarval_from_long(1, mode::Iu))));

	let lb_jmp = loop_body.new_jmp();

	/* goto loop header */
	loop_header.add_pred(lb_jmp);

	/* return block */
	let return_block = graph.new_imm_block(lh_false);
	set_r_cur_block(graph.into(), return_block.into());

	let rb_total: *mut ir_node = graph.get_value(3, mode::Is);
	mem = get_r_store(graph.into());
    let additional_inputs = &[rb_total];
	let rb_return = new_r_Return(return_block.into(), mem, additional_inputs.len() as i32, additional_inputs as *const *mut ir_node);

	add_immBlock_pred(get_irg_end_block(graph.into()), rb_return);

	keep_alive(lb_store);

	dump_ir_graph(graph.into(), cstr!("immature\0"));

	mature_immBlock(start_block.into());
	mature_immBlock(loop_header.into());
	mature_immBlock(loop_body.into());
	mature_immBlock(return_block.into());
	irg_finalize_cons(graph.into());

	dump_ir_graph(graph.into(), cstr!("ssa-constructed\0"));

	lower_highlevel_graph(graph.into());
	be_lower_for_target();

	dump_ir_graph(graph.into(), cstr!("lowered\0"));

	be_main(stdout, cstr!("<stdin>\0"));

	ir_finish();


}
