set -u

skip_files=(
# todo
chapter_1/invalid_lex/backslash.c
chapter_18/invalid_types/extra_credit/invalid_union_lvalues/assign_non_lvalue_union_member.c
chapter_18/invalid_types/invalid_lvalues/assign_nested_non_lvalue.c
chapter_18/invalid_types/invalid_lvalues/assign_to_non_lvalue.c

# c++
chapter_8/invalid_parse/invalid_for_declaration.c

# c2y
chapter_6/invalid_semantics/extra_credit/duplicate_labels.c
chapter_8/invalid_semantics/extra_credit/duplicate_label_in_loop.c
chapter_8/invalid_semantics/extra_credit/duplicate_label_in_default.c
)

c23_allow=(
chapter_6/invalid_parse/extra_credit/label_declaration.c
chapter_6/invalid_parse/extra_credit/label_without_statement.c
chapter_8/invalid_parse/extra_credit/switch_case_declaration.c
chapter_10/invalid_types/static_for_loop_counter.c
chapter_15/invalid_parse/empty_initializer_list.c
chapter_18/invalid_parse/empty_initializer_list.c
chapter_18/invalid_parse/extra_credit/case_struct_decl.c
chapter_18/invalid_parse/extra_credit/labeled_struct_decl.c
chapter_18/invalid_parse/extra_credit/union_empty_initializer.c
chapter_18/invalid_types/tag_resolution/address_of_wrong_type.c
)

old_gcc_allow=(
chapter_9/invalid_types/assign_fun_to_variable.c
chapter_14/invalid_types/assign_wrong_pointer_type.c
chapter_14/invalid_types/pass_pointer_as_int.c
chapter_14/invalid_types/return_wrong_pointer_type.c
chapter_14/invalid_types/ternary_mixed_pointer_types.c
chapter_15/invalid_types/compare_explicit_and_implict_addr.c
chapter_15/invalid_types/assign_incompatible_pointer_types.c
chapter_15/invalid_types/bad_arg_type.c
chapter_16/invalid_types/implicit_conversion_pointers_to_different_size_arrays.c
chapter_17/invalid_types/pointer_conversions/convert_void_ptr_to_int.c
chapter_17/invalid_types/void/no_return_value.c
chapter_17/invalid_types/void/non_void_return.c
chapter_18/invalid_types/extra_credit/incompatible_union_types/union_pointer_branch_mismatch.c
chapter_18/invalid_types/extra_credit/union_tag_resolution/address_of_wrong_union_type.c
chapter_18/invalid_types/incompatible_types/assign_different_pointer_type.c
chapter_18/invalid_types/incompatible_types/compare_different_struct_pointers.c
chapter_18/invalid_types/incompatible_types/struct_pointer_param_mismatch.c
)

gcc_allow=(
chapter_9/invalid_parse/fun_decl_for_loop.c
chapter_14/invalid_types/compare_pointer_to_ulong.c
chapter_14/invalid_types/compare_mixed_pointer_types.c
chapter_15/invalid_types/compare_pointer_to_zero.c
chapter_15/invalid_types/compare_pointer_to_int.c
chapter_15/invalid_types/compound_inititializer_too_long.c
chapter_15/invalid_types/compound_initializer_too_long_static.c
chapter_15/invalid_types/compare_different_pointer_types.c
chapter_16/invalid_lex/string_bad_escape_sequence.c
chapter_16/invalid_lex/char_bad_escape_sequence.c
chapter_16/invalid_types/string_initializer_too_long.c
chapter_16/invalid_types/string_initializer_too_long_static.c
chapter_16/invalid_types/string_initializer_too_long_nested_static.c
chapter_16/invalid_types/string_initializer_too_long_nested.c
chapter_16/invalid_types/string_literal_is_plain_char_pointer_static.c
chapter_16/invalid_types/string_literal_is_plain_char_pointer.c
chapter_16/invalid_types/implicit_conversion_between_char_pointers.c
chapter_17/invalid_types/extra_credit/compound_add_void_pointer.c
chapter_17/invalid_types/extra_credit/compound_sub_void_pointer.c
chapter_17/invalid_types/extra_credit/postfix_decr_void_pointer.c
chapter_17/invalid_types/extra_credit/postfix_incr_void_pointer.c
chapter_17/invalid_types/extra_credit/prefix_decr_void_pointer.c
chapter_17/invalid_types/extra_credit/prefix_incr_void_pointer.c
chapter_17/invalid_types/incomplete_types/add_void_pointer.c
chapter_17/invalid_types/incomplete_types/sizeof_function.c
chapter_17/invalid_types/incomplete_types/sizeof_void.c
chapter_17/invalid_types/incomplete_types/sizeof_void_expression.c
chapter_17/invalid_types/incomplete_types/subscript_void.c
chapter_17/invalid_types/incomplete_types/sub_void_pointer.c
chapter_17/invalid_types/pointer_conversions/compare_void_ptr_to_int.c
chapter_17/invalid_types/pointer_conversions/compare_void_to_other_pointer.c
chapter_17/invalid_types/void/mismatched_conditional.c
chapter_18/invalid_parse/extra_credit/union_decl_empty_member_list.c
chapter_18/invalid_parse/extra_credit/union_decl_extra_semicolon.c
chapter_18/invalid_parse/extra_credit/union_member_no_declarator.c
chapter_18/invalid_parse/struct_decl_double_semicolon.c
chapter_18/invalid_parse/struct_decl_empty_member_list.c
chapter_18/invalid_parse/struct_decl_extra_semicolon.c
chapter_18/invalid_parse/struct_member_no_declarator.c
chapter_18/invalid_types/extra_credit/scalar_required/cast_between_unions.c
chapter_18/invalid_types/extra_credit/union_initializers/initializer_too_long.c
chapter_18/invalid_types/extra_credit/union_initializers/nested_union_init_too_long.c
chapter_18/invalid_types/extra_credit/union_initializers/static_aggregate_init_wrong_type.c
chapter_18/invalid_types/extra_credit/union_initializers/static_nested_init_too_long.c
chapter_18/invalid_types/extra_credit/union_initializers/static_too_long.c
chapter_18/invalid_types/extra_credit/union_initializers/static_union_init_wrong_type.c
chapter_18/invalid_types/incompatible_types/branch_mismatch_2.c
chapter_18/invalid_types/incompatible_types/compare_different_struct_pointers.c
chapter_18/invalid_types/initializers/compound_initializer_too_long.c
chapter_18/invalid_types/initializers/initialize_struct_member_wrong_type.c
chapter_18/invalid_types/initializers/nested_compound_initializer_too_long.c
chapter_18/invalid_types/initializers/nested_static_compound_initializer_too_long.c
chapter_18/invalid_types/initializers/static_initializer_too_long.c
chapter_18/invalid_types/scalar_required/cast_to_struct.c
chapter_18/invalid_types/tag_resolution/shadow_struct.c
chapter_18/invalid_types/extra_credit/union_tag_resolution/compare_struct_and_union_ptrs.c
)

verify_skips() {
  for src in ${old_gcc_allow[@]}; do
    gcc $src -fsyntax-only -std=c99 -Wno-incompatible-pointer-types -Wno-int-conversion -Wno-return-mismatch
    if [ $? -ne 0 ]; then
      echo "$src"
      exit 1
    fi
  done

  for src in ${gcc_allow[@]}; do
    gcc $src -fsyntax-only -std=c99
    if [ $? -ne 0 ]; then
      echo "$src"
      exit 1
    fi
  done

  for src in ${c23_allow[@]}; do
    gcc $src -fsyntax-only -std=c23 -pedantic-errors
    if [ $? -ne 0 ]; then
      echo "$src"
      exit 1
    fi
  done
}

match_skip() {
  local -n arr=$2
  for f in "${arr[@]}"; do
    if [ $f == $1 ];  then
      return 0
    fi
  done
  return 1
}

test_invalid() {
  for n in `seq 1 18`; do
    echo "chapter $n invalid cases:"

    for src in `find chapter_$n | sort | grep '/invalid_.*\.c$'`; do
      if match_skip $src old_gcc_allow; then continue; fi
      if match_skip $src gcc_allow; then continue; fi
      if match_skip $src c23_allow; then continue; fi
      if match_skip $src skip_files; then continue; fi

      $CC $src -S -o/dev/null 2>./warn_log.txt
      if [ $? -eq 0 ]; then
        echo "$src didn't fail"
        exit 1
      fi
      grep 'internal error' ./warn_log.txt
      if [ $? -eq 0 ]; then
        echo "$src failed with ICE"
        exit 1
      fi
      grep AddressSanitizer ./warn_log.txt
      if [ $? -eq 0 ]; then
        echo "$src failed with ASAN"
        exit 1
      fi
    done

    echo 'OK'
  done
}

make_driver() {
  cat << 'EOF' | sed -e "s|CC|$CC|g" > wacc_adapter.sh
#!/bin/sh

if [ "$#" -eq 1 ]; then
  out=`echo $1 | sed -e 's|.c$||g'`
  CC $1 -o $out
  exit $?
fi

if [ "$#" -eq 2 ]; then
  out=`echo $2 | sed -e 's|.c$||g'`
  case $1 in
    '-c')
      CC $2 -c -o $out.o
      exit $?
    ;;
    '-S')
      CC $2 -S -o $out.s
      exit $?
    ;;
    '-lm')
      CC $2 -lm -o $out
      exit $?
    ;;
  esac
fi

echo $@ >&2
exit 1
EOF

  chmod +x $PWD/wacc_adapter.sh
}

main_test() {
  for n in `seq 1 18`; do
    echo "chapter $n:"
    CC=$CC ../test_compiler $PWD/wacc_adapter.sh --failfast --chapter $n --skip-invalid --latest-only --extra-credit
    if [ $? -ne 0 ]; then
      exit 1
    fi
  done
}

cd tests

#verify_skips
test_invalid
make_driver
main_test
