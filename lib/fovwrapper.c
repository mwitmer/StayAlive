#include <libguile.h>
#include <fov.h>

bool bit_vec_ref(SCM bit_vec, int index) {
  scm_t_array_handle handle;
  size_t i, offp, lenp;
  ssize_t inc;
  scm_t_uint32 * bits;

  bits = scm_bitvector_writable_elements(bit_vec, &handle, &offp, &lenp, &inc);

  ssize_t pos = scm_array_handle_pos(&handle, scm_list_1(scm_from_int(index)));
  size_t abs_pos = pos + scm_array_handle_bit_elements_offset(&handle);
  size_t word_pos = abs_pos / 32;
  size_t mask = 1L << (abs_pos % 32);

  bool res = bits[word_pos] & mask;

  scm_array_handle_release(&handle);

  return res;
}

void bit_vec_set(SCM bit_vec, int index) {
  scm_t_array_handle handle;
  size_t i, offp, lenp;
  ssize_t inc;
  scm_t_uint32 * bits;

  bits = scm_bitvector_writable_elements(bit_vec, &handle, &offp, &lenp, &inc);

  ssize_t pos = scm_array_handle_pos(&handle, scm_list_1(scm_from_int(index)));
  size_t abs_pos = pos + scm_array_handle_bit_elements_offset(&handle);
  size_t word_pos = abs_pos / 32;
  size_t mask = 1L << (abs_pos % 32);

  bits[word_pos] |= mask;

  scm_array_handle_release(&handle);
}

bool opacity_test(void * map, int x, int y) {
  return scm_to_bool(scm_call_2((SCM)map, scm_from_int(y), scm_from_int(x)));
}

void lighting_function(void * map, int x, int y, int dx, int dy, void * src) {
  scm_call_2((SCM)src, scm_from_int(y), scm_from_int(x));
}

SCM line_of_sight(SCM map, SCM source, SCM center, SCM radius, SCM apply_to_opaque) {
  fov_settings_type settings;

  fov_settings_init(&settings);

  fov_settings_set_opaque_apply(&settings, scm_to_bool(apply_to_opaque) ? FOV_OPAQUE_APPLY : FOV_OPAQUE_NOAPPLY);
  fov_settings_set_shape(&settings, FOV_SHAPE_CIRCLE);

  fov_settings_set_opacity_test_function(&settings, opacity_test);
  fov_settings_set_apply_lighting_function(&settings, lighting_function);

  fov_circle(&settings, (void*)map, (void*)source, scm_to_int(SCM_CAR(SCM_CDR(center))), scm_to_int(SCM_CAR(center)), scm_to_int(radius));
  
  fov_settings_free(&settings);

  lighting_function((void*)map, scm_to_int(SCM_CAR(SCM_CDR(center))), scm_to_int(SCM_CAR(center)), 1, 1, (void*)source);

  return SCM_UNSPECIFIED;
}

int cols;

bool simple_opacity_test(void * map, int x, int y) {
  return bit_vec_ref((SCM)map, y * cols + x);
}

bool simple_opacity_and_lighting_test(void * map, int x, int y) {
  return bit_vec_ref((SCM)SCM_CAR(map), y * cols + x);
}

void opacity_and_lighting_function(void * map, int x, int y, int dx, int dy, void * src) {
  SCM lighting = SCM_CDR((SCM)map);
  if(bit_vec_ref(lighting, y * cols + x)) {
    bit_vec_set((SCM)src, y * cols + x);
  }
}

void simple_lighting_function(void * map, int x, int y, int dx, int dy, void * src) {
  bit_vec_set((SCM)src, y * cols + x);
}

SCM see_level(SCM opacity, SCM light_sources, SCM radius, SCM center, SCM size) {
  int rows = scm_to_int(SCM_CAR(size));
  cols = scm_to_int(SCM_CAR(SCM_CDR(size)));

  SCM lighting = scm_make_bitvector(scm_from_int(rows * cols), SCM_BOOL_F);

  fov_settings_type settings;

  fov_settings_init(&settings);

  fov_settings_set_opaque_apply(&settings, FOV_OPAQUE_APPLY);
  fov_settings_set_shape(&settings, FOV_SHAPE_CIRCLE);

  fov_settings_set_opacity_test_function(&settings, simple_opacity_test);
  fov_settings_set_apply_lighting_function(&settings, simple_lighting_function);

  while(!scm_is_null(light_sources)) {
    SCM source = SCM_CAR(light_sources);
    SCM location = SCM_CAR(source);
    SCM radius = SCM_CDR(source);

    SCM source_row = SCM_CAR(location);
    SCM source_col = SCM_CAR(SCM_CDR(location));

    fov_circle(&settings, (void*)opacity, (void*)lighting, scm_to_int(source_col), scm_to_int(source_row), scm_to_int(radius));
    simple_lighting_function((void*)opacity, scm_to_int(source_col), scm_to_int(source_row), 1, 1, (void*)lighting);

    light_sources = SCM_CDR(light_sources);
  }

  SCM visibility = scm_make_bitvector(scm_from_int(rows * cols), SCM_BOOL_F);

  bit_vec_set(visibility, scm_to_int(SCM_CAR(center)) * cols + scm_to_int(SCM_CAR(SCM_CDR(center))));

  fov_settings_set_apply_lighting_function(&settings, opacity_and_lighting_function);
  fov_settings_set_opacity_test_function(&settings, simple_opacity_and_lighting_test);
  fov_circle(&settings, (void*)scm_cons(opacity, lighting), (void*)visibility, scm_to_int(SCM_CAR(SCM_CDR(center))), scm_to_int(SCM_CAR(center)), scm_to_int(radius));

  fov_settings_free(&settings);

  SCM result = scm_cons(lighting, visibility);

  return result;
}

void init_fov() {
  scm_c_define_gsubr("line-of-sight!", 5, 0, 0, line_of_sight);
  scm_c_define_gsubr("see-level", 5, 0, 0, see_level);
}
