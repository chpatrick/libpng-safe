#pragma once

#include "png.h"

void safe_error_shim(png_structp png_struct, png_const_charp message);

void safe_create_read_struct(png_error_ptr error_fn, png_error_ptr warn_fn);

