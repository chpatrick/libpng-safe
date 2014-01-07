#include "libpng-safe.h"

void safe_error_shim(png_structp png_ptr, png_const_charp msg) {
  png_error_ptr managed_error = png_get_error_ptr(png_ptr);
  managed_error(png_ptr, msg);
  png_longjmp(png_ptr, 0);
};

void safe_create_read_struct(png_error_ptr error_fn, png_error_ptr warn_fn) {
  // Pass the managed error callback as the data for the safe error shim.
  png_create_read_struct(PNG_LIBPNG_VER_STRING, error_fn, safe_error_shim, warn_fn);
};

// Returning wrapper for libpng functions that can fail.
// If there was no error, the return value is returned.
// Otherwise, an undefined value is returned. The error handler must handle this.
#define SHIM(ret, func, def, args)\
ret SHIM_##func def{\
  ret result;\
  if (setjmp(png_jmpbuf(png_ptr)) == 0) {\
    result = func args;\
  }\
  return result;\
}

#define SHIM_VOID(func, def, args)\
void SHIM_##func def{\
  if (setjmp(png_jmpbuf(png_ptr)) == 0) {\
    func args;\
  }\
}

SHIM(png_infop, png_create_info_struct, (png_structp png_ptr), (png_ptr));
SHIM_VOID(png_error, (png_structp png_ptr, png_const_charp msg), (png_ptr, msg));