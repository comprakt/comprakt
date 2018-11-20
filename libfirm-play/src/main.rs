use libfirm_rs_bindings::*;

use std::ffi::CString;
use std::os::raw::c_char;

pub fn to_const_c_char(s: &str) -> *const c_char {
    CString::new(s).unwrap().as_ptr()
}

pub fn to_mut_c_char(s: &str) -> *mut c_char {
    to_const_c_char(s) as *mut c_char
}

// class Foo {
//     public static void main(String[] args) { }
//
//     public int running_sum(int[] values, int[] output, int length) {
//         int total = 0;
//         int i = 0;
//         while (i < length) {
//             total = total + values[i];
//             output[i] = total;
//             i = i + 1;
//         }
//         return total;
//     }
// }

fn main() {
    unsafe {
        // Setup
        ir_init_library();

        let triple = ir_get_host_machine_triple();
        ir_target_set_triple(triple);
        ir_target_option(to_const_c_char("dump=all"));

        ir_target_init();

        set_optimize(0);

        let int_ty = new_type_primitive(mode_Is);
        let int_array_ty = new_type_array(int_ty, 0);
        let foo_class = new_type_class(to_mut_c_char("Foo"));

        // Function
        let running_sum_ty = new_type_method(
            4,
            1,
            0,
            calling_convention_cc_this_call,
            mtp_additional_properties_mtp_no_property,
        );
        set_method_param_type(running_sum_ty, 0, foo_class);
        set_method_param_type(running_sum_ty, 1, new_type_pointer(int_ty));
        set_method_param_type(running_sum_ty, 2, new_type_pointer(int_ty));
        set_method_param_type(running_sum_ty, 3, int_ty);
        set_method_res_type(running_sum_ty, 0, int_ty);

        // register function as global
        let global_ty = get_glob_type();
        let name = new_id_from_str(to_const_c_char("running_sum"));
        let running_sum = new_entity(global_ty, name, running_sum_ty);

        //setup graph
        let n_slots = 6; // this, values, output, length, total, i
        let graph = new_ir_graph(running_sum, n_slots);
        set_current_ir_graph(graph);

        let start_block = get_irg_start_block(graph);
        set_cur_block(start_block);

        // Setup method params
        set_value(1, new_Proj(get_irg_args(graph), mode_P, 1));
        set_value(2, new_Proj(get_irg_args(graph), mode_P, 2));
        set_value(3, new_Proj(get_irg_args(graph), mode_Is, 3));

        // Setup local vars
        let total = new_Const(new_tarval_from_long(0, mode_Is));
        set_value(4, total);
        let i = new_Const(new_tarval_from_long(0, mode_Is));
        set_value(5, i);

        // Basic blocks of method
        let start_jmp = new_Jmp();

        let loop_header = new_immBlock();
        add_immBlock_pred(loop_header, start_jmp);
        set_cur_block(loop_header);

        let i_val = get_value(5, mode_Is);
        let length_val = get_value(3, mode_Is);
        let cmp_res = new_Cmp(i_val, length_val, ir_relation_ir_relation_less);

        let cond = new_Cond(cmp_res);
        let true_jmp = new_Proj(cond, mode_X, pn_Cond_pn_Cond_true);
        let false_jmp = new_Proj(cond, mode_X, pn_Cond_pn_Cond_false);

        let loop_body = new_immBlock();
        add_immBlock_pred(loop_body, true_jmp);
        set_cur_block(loop_body);

        let values = get_value(1, mode_P);
        let i_val = get_value(5, mode_Is);
        let values_i_ptr = new_Sel(values, i_val, int_array_ty);
        let mem = get_store(); // current memory state
        let load = new_Load(mem, values_i_ptr, mode_Is, int_ty, ir_cons_flags_cons_none); // load from memory
        set_store(new_Proj(load, mode_M, pn_Load_pn_Load_M));

        let total = get_value(4, mode_Is);
        set_value(4, new_Add(total, new_Proj(load, mode_Is, pn_Load_pn_Load_res)));

        let total = get_value(4, mode_Is);
        let i_val = get_value(5, mode_Is);
        let output = get_value(2, mode_P);
        let output_i_ptr = new_Sel(output, i_val, int_array_ty);
        let mem = get_store();
        let store = new_Store(mem, output_i_ptr, total, int_ty, ir_cons_flags_cons_none);
        set_store(new_Proj(store, mode_M, pn_Store_pn_Store_M));

        let i_val = get_value(5, mode_Is);
        set_value(5, new_Add(i_val, new_Const(new_tarval_from_long(0, mode_Is))));

        let jmp = new_Jmp();
        add_immBlock_pred(loop_header, jmp);

        let loop_end = new_immBlock();
        add_immBlock_pred(loop_end, false_jmp);
        set_cur_block(loop_end);

        let total = get_value(4, mode_Is);
        let mem = get_store();
        let additional_inputs = &[total];
        let return_ = new_Return(mem, 1, additional_inputs as *const *mut ir_node);

        add_immBlock_pred(get_irg_end_block(graph), return_);

        mature_immBlock(start_block);
        mature_immBlock(loop_header);
        mature_immBlock(loop_body);
        mature_immBlock(loop_end);
        irg_finalize_cons(graph);

        dump_ir_graph(graph, to_const_c_char("ssa-constructed"));

        lower_highlevel_graph(graph);
        be_lower_for_target();

        dump_ir_graph(graph, to_const_c_char("lowered"));

        be_main(stdout, to_const_c_char("<stdin>"));

        ir_finish();
    };
}
