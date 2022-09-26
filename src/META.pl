#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Specifications for the "hygienic-macros" package:
version = "$Version::version"
description = "Hygienic Macros"

archive(byte) = "pa_hmac.cmo"
archive(native) = "pa_hmac.cmx"
requires = "fmt,camlp5.extend,camlp5.parser_quotations,camlp5.pa_r.link"

EOF
