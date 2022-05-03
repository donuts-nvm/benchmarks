/* ANSI-C code produced by genperf */
/* Command-line: genperf /home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf x86insn_nasm.c */
#line 9 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
struct insnprefix_parse_data;
static const struct insnprefix_parse_data *
insnprefix_nasm_find(const char *key, size_t len)
{
  static const struct insnprefix_parse_data pd[1434] = {
#line 176 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngtss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 1301 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilps",	vpermil_insn,	4,	NONE,	0x04,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 616 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomeqq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 1047 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgt_oqps",	ssecmp_128_insn,	3,	NONE,	0x1E,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 100 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpltpd",	ssecmp_128_insn,	3,	NONE,	0x01,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 543 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsx",	movszx_insn,	5,	NONE,	0xBE,	0,	0,	0,	CPU_386,	0,	0},
#line 821 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"protw",	sse5prot_insn,	3,	NONE,	0x01,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1214 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vminss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 336 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldl2e",	twobyte_insn,	1,	NONE,	0xD9,	0xEA,	0,	0,	CPU_FPU,	0,	0},
#line 1218 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovapd",	movau_insn,	6,	NONE,	0x66,	0x28,	0x01,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1187 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmaddps",	fma_128_256_insn,	4,	NONE,	0x78,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1266 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xFC,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 179 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnlesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 895 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ror",	shift_insn,	16,	NONE,	0x01,	0,	0,	0,	0,	0,	0},
#line 729 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phadduwq",	sse5two_insn,	1,	NONE,	0x57,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 672 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneqw",	sse5comcc_insn,	1,	NONE,	0x4D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 880 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rcr",	shift_insn,	16,	NONE,	0x03,	0,	0,	0,	0,	0,	0},
#line 158 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comltps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 893 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"retn",	retnf_insn,	6,	NONE,	0xC2,	0,	0,	0,	0,	0,	0},
#line 201 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 370 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fptan",	twobyte_insn,	1,	NONE,	0xD9,	0xF2,	0,	0,	CPU_FPU,	0,	0},
#line 977 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"subsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5C,	0,	0,	CPU_SSE2,	0,	0},
#line 537 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsd",	movsd_insn,	5,	NONE,	0,	0,	0,	0,	CPU_386,	0,	0},
#line 1173 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vextractps",	extractps_insn,	2,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1109 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnless",	ssecmp_32_insn,	4,	NONE,	0x06,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1361 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsignw",	ssse3_insn,	3,	NONE,	0x09,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 903 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsqrtps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x52,	0,	0,	CPU_SSE,	0,	0},
#line 610 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpistri",	sse4pcmpstr_insn,	1,	NONE,	0x63,	0,	0,	0,	CPU_SSE42,	0,	0},
#line 642 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1323 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxsw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xEE,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 157 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comltpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 1401 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsqrtps",	avx_xmm_xmm128_insn,	2,	NONE,	0x00,	0x51,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 688 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomud",	sse5com_insn,	1,	NONE,	0x6E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 25 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addsubps",	xmm_xmm128_insn,	2,	NONE,	0xF2,	0xD0,	0,	0,	CPU_SSE3,	0,	0},
#line 982 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"swapgs",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xF8,	ONLY_64,	0,	0,	0},
#line 1374 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xFB,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1025 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_usss",	ssecmp_32_insn,	4,	NONE,	0x18,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 257 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtps2pd",	xmm_xmm64_insn,	4,	NONE,	0x00,	0x5A,	0,	0,	CPU_SSE2,	0,	0},
#line 745 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pinsrw",	pinsrw_insn,	9,	NONE,	0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 741 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pi2fw",	now3d_insn,	1,	NONE,	0x0C,	0,	0,	0,	CPU_3DNow,	CPU_Athlon,	0},
#line 875 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushfw",	onebyte_insn,	1,	NONE,	0x9C,	0x10,	0x40,	0,	0,	0,	0},
#line 1045 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgess",	ssecmp_32_insn,	4,	NONE,	0x0D,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 684 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 877 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rcl",	shift_insn,	16,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 969 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stosb",	onebyte_insn,	1,	NONE,	0xAA,	0x00,	0,	0,	0,	0,	0},
#line 1105 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnle_uqss",	ssecmp_32_insn,	4,	NONE,	0x16,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1249 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmsave",	svm_rax_insn,	2,	NONE,	0xDB,	0,	0,	0,	CPU_SVM,	0,	0},
#line 678 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomq",	sse5com_insn,	1,	NONE,	0x4F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1156 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtsi2sd",	cvt_xmm_rmx_insn,	6,	NONE,	0xF2,	0x2A,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 951 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shufpd",	xmm_xmm128_imm_insn,	1,	NONE,	0x66,	0xC6,	0,	0,	CPU_SSE2,	0,	0},
#line 1086 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnge_uqpd",	ssecmp_128_insn,	3,	NONE,	0x19,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 250 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtpd2dq",	xmm_xmm128_insn,	2,	NONE,	0xF2,	0xE6,	0,	0,	CPU_SSE2,	0,	0},
#line 1155 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtsd2ss",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5A,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 914 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"scasw",	onebyte_insn,	1,	NONE,	0xAF,	0x10,	0,	0,	0,	0,	0},
#line 1370 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsrlq",	vpshift_insn,	4,	NONE,	0xD3,	0x73,	0x02,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 373 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frczsd",	sse5two64_insn,	2,	NONE,	0x13,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 842 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psllw",	pshift_insn,	4,	NONE,	0xF1,	0x71,	0x06,	0,	CPU_MMX,	0,	0},
#line 1398 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vshufpd",	xmm_xmm128_imm_256_insn,	3,	NONE,	0x66,	0xC6,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 383 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fst",	fst_insn,	3,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 369 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fprem1",	twobyte_insn,	1,	NONE,	0xD9,	0xF5,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 930 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setne",	setcc_insn,	1,	NONE,	0x05,	0,	0,	0,	CPU_386,	0,	0},
#line 1429 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xcryptcfb",	padlock_insn,	1,	NONE,	0xE0,	0xF3,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 1332 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminuw",	ssse3_insn,	3,	NONE,	0x3A,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 975 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"subpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5C,	0,	0,	CPU_SSE2,	0,	0},
#line 36 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"andps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x54,	0,	0,	CPU_SSE,	0,	0},
#line 1395 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vroundss",	sse4m32imm_insn,	4,	NONE,	0x0A,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 488 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loop",	loop_insn,	8,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 857 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubw",	mmxsse2_insn,	2,	NONE,	0xF9,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1421 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"wait",	onebyte_insn,	1,	NONE,	0x9B,	0,	0,	0,	0,	0,	0},
#line 553 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mulss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x59,	0,	0,	CPU_SSE,	0,	0},
#line 561 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"or",	arith_insn,	22,	NONE,	0x08,	0x01,	0,	0,	0,	0,	0},
#line 972 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stosw",	onebyte_insn,	1,	NONE,	0xAB,	0x10,	0,	0,	0,	0,	0},
#line 1024 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_ussd",	ssecmp_64_insn,	4,	NONE,	0x18,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 484 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lodsb",	onebyte_insn,	1,	NONE,	0xAC,	0x00,	0,	0,	0,	0,	0},
#line 848 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psrlw",	pshift_insn,	4,	NONE,	0xD1,	0x71,	0x02,	0,	CPU_MMX,	0,	0},
#line 28 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aesenc",	aes_insn,	1,	NONE,	0x38,	0xDC,	0,	0,	CPU_AES,	0,	0},
#line 547 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movzx",	movszx_insn,	5,	NONE,	0xB6,	0,	0,	0,	CPU_386,	0,	0},
#line 1269 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddsb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xEC,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1304 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpextrb",	pextrb_insn,	3,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 248 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtdq2pd",	xmm_xmm64_insn,	4,	NONE,	0xF3,	0xE6,	0,	0,	CPU_SSE2,	0,	0},
#line 747 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacsdd",	sse5pmacs_insn,	1,	NONE,	0x9E,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 879 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rcpss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x53,	0,	0,	CPU_SSE,	0,	0},
#line 81 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovno",	cmovcc_insn,	3,	NONE,	0x01,	0,	0,	0,	CPU_686,	0,	0},
#line 686 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtruew",	sse5comcc_insn,	1,	NONE,	0x4D,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 600 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpeqb",	mmxsse2_insn,	2,	NONE,	0x74,	0,	0,	0,	CPU_MMX,	0,	0},
#line 286 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"extractps",	extractps_insn,	2,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_SSE41,	0},
#line 308 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcompp",	twobyte_insn,	1,	NONE,	0xDE,	0xD9,	0,	0,	CPU_FPU,	0,	0},
#line 802 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popa",	onebyte_insn,	1,	NONE,	0x61,	0x00,	0,	NOT_64,	CPU_186,	0,	0},
#line 161 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 323 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fild",	fildstp_insn,	3,	NONE,	0x00,	0x02,	0x05,	0,	CPU_FPU,	0,	0},
#line 760 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaddwd",	mmxsse2_insn,	2,	NONE,	0xF5,	0,	0,	0,	CPU_MMX,	0,	0},
#line 156 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comless",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1200 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vlddqu",	lddqu_insn,	2,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 680 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrued",	sse5comcc_insn,	1,	NONE,	0x4E,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 1082 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneqpd",	ssecmp_128_insn,	3,	NONE,	0x04,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 591 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pavgw",	mmxsse2_insn,	2,	NONE,	0xE3,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 554 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mwait",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC9,	0,	CPU_SSE3,	0,	0},
#line 818 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"protb",	sse5prot_insn,	3,	NONE,	0x00,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 904 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsqrtss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x52,	0,	0,	CPU_SSE,	0,	0},
#line 1190 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmsubpd",	fma_128_256_insn,	4,	NONE,	0x7D,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 887 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"repe",	NULL,	X86_LOCKREP>>8,	0xF3,	0,	0,	0,	0,	0,	0,	0},
#line 27 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aesdeclast",	aes_insn,	1,	NONE,	0x38,	0xDF,	0,	0,	CPU_AES,	0,	0},
#line 1366 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsrad",	vpshift_insn,	4,	NONE,	0xE2,	0x72,	0x04,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1240 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovss",	movss_insn,	4,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 62 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovae",	cmovcc_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_686,	0,	0},
#line 1343 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxdq",	sse4m64_insn,	2,	NONE,	0x35,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 164 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comneqps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 1013 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vbroadcastss",	vbroadcastss_insn,	2,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 604 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpestri",	sse4pcmpstr_insn,	1,	NONE,	0x61,	0,	0,	0,	CPU_SSE42,	0,	0},
#line 606 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpgtb",	mmxsse2_insn,	2,	NONE,	0x64,	0,	0,	0,	CPU_MMX,	0,	0},
#line 289 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fabs",	twobyte_insn,	1,	NONE,	0xD9,	0xE1,	0,	0,	CPU_FPU,	0,	0},
#line 989 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ucomiss",	xmm_xmm32_insn,	4,	NONE,	0x00,	0x2E,	0,	0,	CPU_SSE,	0,	0},
#line 241 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunordpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 85 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovo",	cmovcc_insn,	3,	NONE,	0x00,	0,	0,	0,	CPU_686,	0,	0},
#line 192 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comss",	sse5com32_insn,	2,	NONE,	0x2E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1095 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngt_uqps",	ssecmp_128_insn,	3,	NONE,	0x1A,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 450 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jne",	jcc_insn,	9,	NONE,	0x05,	0,	0,	0,	0,	0,	0},
#line 482 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loadall286",	twobyte_insn,	1,	NONE,	0x0F,	0x05,	0,	0,	CPU_286,	CPU_Undoc,	0},
#line 44 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bsr",	bsfr_insn,	3,	NONE,	0xBD,	0,	0,	0,	CPU_386,	0,	0},
#line 215 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comultsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 734 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubbw",	sse5two_insn,	1,	NONE,	0x61,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 961 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sqrtps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x51,	0,	0,	CPU_SSE,	0,	0},
#line 944 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setz",	setcc_insn,	1,	NONE,	0x04,	0,	0,	0,	CPU_386,	0,	0},
#line 206 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugtps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1176 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddsd",	fma_128_m64_insn,	3,	NONE,	0x6B,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 288 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"f2xm1",	twobyte_insn,	1,	NONE,	0xD9,	0xF0,	0,	0,	CPU_FPU,	0,	0},
#line 367 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fpatan",	twobyte_insn,	1,	NONE,	0xD9,	0xF3,	0,	0,	CPU_FPU,	0,	0},
#line 652 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 923 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setl",	setcc_insn,	1,	NONE,	0x0C,	0,	0,	0,	CPU_386,	0,	0},
#line 92 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpeqpd",	ssecmp_128_insn,	3,	NONE,	0x00,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 963 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sqrtss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x51,	0,	0,	CPU_SSE,	0,	0},
#line 559 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"o32",	NULL,	X86_OPERSIZE>>8,	0x20,	0,	0,	0,	0,	0,	0,	0},
#line 401 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fxrstor",	twobytemem_insn,	1,	NONE,	0x01,	0x0F,	0xAE,	0,	CPU_686,	CPU_FPU,	0},
#line 1232 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovntdq",	movnt_insn,	1,	NONE,	0x66,	0xE7,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1415 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vunpcklpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x14,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 645 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtw",	sse5comcc_insn,	1,	NONE,	0x4D,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 732 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddwq",	sse5two_insn,	1,	NONE,	0x47,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 132 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpxchg16b",	cmpxchg16b_insn,	1,	NONE,	0,	0,	0,	ONLY_64,	0,	0,	0},
#line 1098 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngtpd",	ssecmp_128_insn,	3,	NONE,	0x0A,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1147 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcomiss",	avx_xmm_xmm32_insn,	2,	NONE,	0x00,	0x2F,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1324 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxub",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDE,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 778 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxdq",	sse4m64_insn,	2,	NONE,	0x25,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1066 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpltpd",	ssecmp_128_insn,	3,	NONE,	0x01,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 198 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comueqps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x08,	0,	0,	CPU_SSE5,	0,	0},
#line 1316 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpinsrd",	pinsrd_insn,	2,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1352 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmuludq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xF4,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 765 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxub",	mmxsse2_insn,	2,	NONE,	0xDE,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1259 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpabsb",	avx_ssse3_2op_insn,	1,	NONE,	0x1C,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1003 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vandnpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x55,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 408 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"hlt",	onebyte_insn,	1,	NONE,	0xF4,	0,	0,	0,	CPU_Priv,	0,	0},
#line 693 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"permpd",	sse5arith_insn,	4,	NONE,	0x21,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1030 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalse_ospd",	ssecmp_128_insn,	3,	NONE,	0x1B,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 935 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setno",	setcc_insn,	1,	NONE,	0x01,	0,	0,	0,	CPU_386,	0,	0},
#line 330 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fisub",	fiarith_insn,	2,	NONE,	0x04,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 67 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovg",	cmovcc_insn,	3,	NONE,	0x0F,	0,	0,	0,	CPU_686,	0,	0},
#line 53 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cdqe",	onebyte_insn,	1,	NONE,	0x98,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 1170 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"verr",	prot286_insn,	1,	NONE,	0x04,	0x00,	0,	0,	CPU_286,	CPU_Prot,	0},
#line 1108 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlesd",	ssecmp_64_insn,	4,	NONE,	0x06,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 252 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtpd2ps",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5A,	0,	0,	CPU_SSE2,	0,	0},
#line 187 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comordsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 1172 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vextractf128",	vextractf128_insn,	1,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1154 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtsd2si",	cvt_rx_xmm64_insn,	4,	NONE,	0xF2,	0x2D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 584 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"palignr",	ssse3imm_insn,	2,	NONE,	0x0F,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 1444 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xstorerng",	padlock_insn,	1,	NONE,	0xC0,	0x00,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 129 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpunordsd",	ssecmp_64_insn,	4,	NONE,	0x03,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 1345 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxwq",	sse4m32_insn,	2,	NONE,	0x34,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 394 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fucomi",	fcom2_insn,	2,	NONE,	0xDB,	0xE8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 1051 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgtps",	ssecmp_128_insn,	3,	NONE,	0x0E,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 679 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 13 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"a64",	NULL,	X86_ADDRSIZE>>8,	0x40,	0,	0,	0,	ONLY_64,	0,	0,	0},
#line 1292 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpistrm",	sse4pcmpstr_insn,	1,	NONE,	0x62,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 981 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"svts",	cyrixsmm_insn,	1,	NONE,	0x7C,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 1338 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxwd",	sse4m64_insn,	2,	NONE,	0x23,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 213 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comultpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 1365 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsllw",	vpshift_insn,	4,	NONE,	0xF1,	0x71,	0x06,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 270 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttsd2si",	cvt_rx_xmm64_insn,	4,	NONE,	0xF2,	0x2C,	0,	0,	CPU_SSE2,	0,	0},
#line 444 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jmp",	jmp_insn,	27,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1306 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpextrq",	pextrq_insn,	1,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 88 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovpo",	cmovcc_insn,	3,	NONE,	0x0B,	0,	0,	0,	CPU_686,	0,	0},
#line 411 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ibts",	ibts_insn,	2,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_Obs,	CPU_Undoc},
#line 1008 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vblendps",	sse4imm_256_insn,	3,	NONE,	0x0C,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 500 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maxps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x5F,	0,	0,	CPU_SSE,	0,	0},
#line 799 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmvnzb",	cyrixmmx_insn,	1,	NONE,	0x5A,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 800 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmvzb",	cyrixmmx_insn,	1,	NONE,	0x58,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 588 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paveb",	cyrixmmx_insn,	1,	NONE,	0x50,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 1438 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xrstor",	twobytemem_insn,	1,	NONE,	0x05,	0x0F,	0xAE,	0,	CPU_386,	CPU_XSAVE,	0},
#line 508 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"monitor",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC8,	0,	CPU_SSE3,	0,	0},
#line 1133 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptrue_usss",	ssecmp_32_insn,	4,	NONE,	0x1F,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 766 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxud",	sse4_insn,	2,	NONE,	0x3F,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 217 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 1151 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtpd2ps",	avx_cvt_xmm128_insn,	2,	NONE,	0x66,	0x5A,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 138 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comeqss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 817 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetchw",	twobytemem_insn,	1,	NONE,	0x01,	0x0F,	0x0D,	0,	CPU_3DNow,	0,	0},
#line 474 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lfence",	threebyte_insn,	1,	NONE,	0x0F,	0xAE,	0xE8,	0,	CPU_P3,	0,	0},
#line 116 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpordpd",	ssecmp_128_insn,	3,	NONE,	0x07,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 406 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"haddpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x7C,	0,	0,	CPU_SSE3,	0,	0},
#line 1250 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmulpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x59,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 55 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cld",	onebyte_insn,	1,	NONE,	0xFC,	0,	0,	0,	0,	0,	0},
#line 1112 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlt_uqsd",	ssecmp_64_insn,	4,	NONE,	0x15,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1356 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpshufd",	xmm_xmm128_imm_insn,	1,	NONE,	0x66,	0x70,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 498 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maskmovq",	maskmovq_insn,	1,	NONE,	0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1093 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngess",	ssecmp_32_insn,	4,	NONE,	0x09,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 786 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxwq",	sse4m32_insn,	2,	NONE,	0x34,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1046 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgt_oqpd",	ssecmp_128_insn,	3,	NONE,	0x1E,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1313 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphsubsw",	ssse3_insn,	3,	NONE,	0x07,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1194 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vhaddpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x7C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 718 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddbd",	sse5two_insn,	1,	NONE,	0x42,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 377 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsave",	twobytemem_insn,	1,	NONE,	0x06,	0x9B,	0xDD,	0,	CPU_FPU,	0,	0},
#line 1162 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvttsd2si",	cvt_rx_xmm64_insn,	4,	NONE,	0xF2,	0x2C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 622 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 37 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"arpl",	arpl_insn,	1,	NONE,	0,	0,	0,	NOT_64,	CPU_286,	CPU_Prot,	0},
#line 359 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmsubps",	sse5arith_insn,	4,	NONE,	0x18,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 872 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushf",	onebyte_insn,	1,	NONE,	0x9C,	0x00,	0x40,	0,	0,	0,	0},
#line 169 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 149 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgtsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 775 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxbd",	sse4m32_insn,	2,	NONE,	0x21,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 668 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomnequb",	sse5comcc_insn,	1,	NONE,	0x6C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 152 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comiss",	xmm_xmm32_insn,	4,	NONE,	0x00,	0x2F,	0,	0,	CPU_SSE,	0,	0},
#line 1440 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xsetbv",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xD1,	0,	CPU_386,	CPU_Priv,	CPU_XSAVE},
#line 932 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnge",	setcc_insn,	1,	NONE,	0x0C,	0,	0,	0,	CPU_386,	0,	0},
#line 120 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmppd",	xmm_xmm128_imm_insn,	1,	NONE,	0x66,	0xC2,	0,	0,	CPU_SSE2,	0,	0},
#line 346 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmsubpd",	sse5arith_insn,	4,	NONE,	0x09,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 991 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ud2",	twobyte_insn,	1,	NONE,	0x0F,	0x0B,	0,	0,	CPU_286,	0,	0},
#line 237 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunltpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 1360 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsignd",	ssse3_insn,	3,	NONE,	0x0A,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1272 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddusw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDD,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1318 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpinsrw",	pinsrw_insn,	9,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1201 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vldmxcsr",	ldstmxcsr_insn,	1,	NONE,	0x02,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 609 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpgtw",	mmxsse2_insn,	2,	NONE,	0x65,	0,	0,	0,	CPU_MMX,	0,	0},
#line 130 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpunordss",	ssecmp_32_insn,	4,	NONE,	0x03,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 568 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pabsb",	ssse3_insn,	3,	NONE,	0x1C,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 477 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lgs",	lfgss_insn,	2,	NONE,	0xB5,	0,	0,	0,	CPU_386,	0,	0},
#line 389 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsubp",	farithp_insn,	3,	NONE,	0xE8,	0,	0,	0,	CPU_FPU,	0,	0},
#line 439 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jecxz",	jcxz_insn,	2,	NONE,	0x20,	0,	0,	0,	CPU_386,	0,	0},
#line 1211 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vminpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x5D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1203 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaskmovpd",	vmaskmov_insn,	4,	NONE,	0x2D,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 325 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fincstp",	twobyte_insn,	1,	NONE,	0xD9,	0xF7,	0,	0,	CPU_FPU,	0,	0},
#line 16 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aam",	aadm_insn,	2,	NONE,	0x00,	0,	0,	NOT_64,	0,	0,	0},
#line 563 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"orps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x56,	0,	0,	CPU_SSE,	0,	0},
#line 1417 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vxorpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x57,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1207 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaxsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5F,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 710 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfpnacc",	now3d_insn,	1,	NONE,	0x8E,	0,	0,	0,	CPU_3DNow,	CPU_Athlon,	0},
#line 374 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frczss",	sse5two32_insn,	2,	NONE,	0x12,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1061 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpless",	ssecmp_32_insn,	4,	NONE,	0x02,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 753 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacsswd",	sse5pmacs_insn,	1,	NONE,	0x86,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 159 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comltsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 83 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovns",	cmovcc_insn,	3,	NONE,	0x09,	0,	0,	0,	CPU_686,	0,	0},
#line 1104 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnle_uqsd",	ssecmp_64_insn,	4,	NONE,	0x16,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1383 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpckhqdq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 229 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungtpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 541 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movss",	movss_insn,	4,	NONE,	0,	0,	0,	0,	CPU_SSE,	0,	0},
#line 31 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aeskeygenassist",	aes_imm_insn,	1,	NONE,	0x3A,	0xDF,	0,	0,	CPU_AES,	0,	0},
#line 995 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"unpcklpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x14,	0,	0,	CPU_SSE2,	0,	0},
#line 578 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddsb",	mmxsse2_insn,	2,	NONE,	0xEC,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1405 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsubpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x5C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 421 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"int",	int_insn,	1,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 809 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popfw",	onebyte_insn,	1,	NONE,	0x9D,	0x10,	0x40,	0,	0,	0,	0},
#line 1330 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminub",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDA,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 275 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"das",	onebyte_insn,	1,	NONE,	0x2F,	0,	0,	NOT_64,	0,	0,	0},
#line 1120 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpord_ssd",	ssecmp_64_insn,	4,	NONE,	0x17,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 131 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpxchg",	cmpxchgxadd_insn,	4,	NONE,	0xB0,	0,	0,	0,	CPU_486,	0,	0},
#line 979 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"svdc",	svdc_insn,	1,	NONE,	0,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 690 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomuw",	sse5com_insn,	1,	NONE,	0x6D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 739 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubwd",	sse5two_insn,	1,	NONE,	0x62,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 650 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 959 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"smsw",	sldtmsw_insn,	6,	NONE,	0x04,	0x01,	0,	0,	CPU_286,	0,	0},
#line 470 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lds",	ldes_insn,	2,	NONE,	0xC5,	0,	0,	NOT_64,	0,	0,	0},
#line 295 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fclex",	threebyte_insn,	1,	NONE,	0x9B,	0xDB,	0xE2,	0,	CPU_FPU,	0,	0},
#line 324 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fimul",	fiarith_insn,	2,	NONE,	0x01,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 983 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"syscall",	twobyte_insn,	1,	NONE,	0x0F,	0x05,	0,	0,	CPU_686,	CPU_AMD,	0},
#line 347 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmsubps",	sse5arith_insn,	4,	NONE,	0x08,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1296 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilmo2pd",	vpermil2_fixed_insn,	4,	NONE,	0x49,	0x02,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 860 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpckhbw",	mmxsse2_insn,	2,	NONE,	0x68,	0,	0,	0,	CPU_MMX,	0,	0},
#line 29 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aesenclast",	aes_insn,	1,	NONE,	0x38,	0xDD,	0,	0,	CPU_AES,	0,	0},
#line 368 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fprem",	twobyte_insn,	1,	NONE,	0xD9,	0xF8,	0,	0,	CPU_FPU,	0,	0},
#line 1157 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtsi2ss",	cvt_xmm_rmx_insn,	6,	NONE,	0xF3,	0x2A,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 565 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"outsb",	onebyte_insn,	1,	NONE,	0x6E,	0x00,	0,	0,	0,	0,	0},
#line 1076 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_ossd",	ssecmp_64_insn,	4,	NONE,	0x1C,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1382 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpckhdq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6A,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 133 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpxchg486",	cmpxchgxadd_insn,	4,	NONE,	0xA6,	0,	0,	0,	CPU_486,	CPU_Undoc,	0},
#line 836 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psignb",	ssse3_insn,	3,	NONE,	0x08,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 339 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldln2",	twobyte_insn,	1,	NONE,	0xD9,	0xED,	0,	0,	CPU_FPU,	0,	0},
#line 660 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 86 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovp",	cmovcc_insn,	3,	NONE,	0x0A,	0,	0,	0,	CPU_686,	0,	0},
#line 1286 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpestrm",	sse4pcmpstr_insn,	1,	NONE,	0x60,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 363 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnsave",	onebytemem_insn,	1,	NONE,	0x06,	0xDD,	0,	0,	CPU_FPU,	0,	0},
#line 1220 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovd",	vmovd_insn,	2,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_386,	CPU_AVX,	0},
#line 185 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comordpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 491 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loopnz",	loop_insn,	8,	NONE,	0x00,	0,	0,	0,	0,	0,	0},
#line 1141 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunord_sss",	ssecmp_32_insn,	4,	NONE,	0x13,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 505 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"minps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x5D,	0,	0,	CPU_SSE,	0,	0},
#line 510 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mov",	mov_insn,	69,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1171 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"verw",	prot286_insn,	1,	NONE,	0x05,	0x00,	0,	0,	CPU_286,	CPU_Prot,	0},
#line 1114 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnltpd",	ssecmp_128_insn,	3,	NONE,	0x05,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 697 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pextrq",	pextrq_insn,	1,	NONE,	0,	0,	0,	ONLY_64,	CPU_SSE41,	0,	0},
#line 783 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxbw",	sse4m64_insn,	2,	NONE,	0x30,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1217 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmmcall",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xD9,	0,	CPU_SVM,	0,	0},
#line 1358 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpshuflw",	xmm_xmm128_imm_insn,	1,	NONE,	0xF2,	0x70,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 422 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"int03",	onebyte_insn,	1,	NONE,	0xCC,	0,	0,	0,	0,	0,	0},
#line 1059 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpleps",	ssecmp_128_insn,	3,	NONE,	0x02,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 139 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comfalsepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0B,	0,	0,	CPU_SSE5,	0,	0},
#line 1263 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpacksswb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x63,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1175 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddps",	fma_128_256_insn,	4,	NONE,	0x68,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 593 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pblendw",	sse4imm_insn,	2,	NONE,	0x0E,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 999 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x58,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1340 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxbd",	sse4m32_insn,	2,	NONE,	0x31,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 949 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shr",	shift_insn,	16,	NONE,	0x05,	0,	0,	0,	0,	0,	0},
#line 958 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"smintold",	twobyte_insn,	1,	NONE,	0x0F,	0x7E,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_Obs},
#line 1208 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaxss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5F,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1116 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnltsd",	ssecmp_64_insn,	4,	NONE,	0x05,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 345 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmaddss",	sse5arith32_insn,	8,	NONE,	0x02,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 659 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1437 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xorps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x57,	0,	0,	CPU_SSE,	0,	0},
#line 694 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"permps",	sse5arith_insn,	4,	NONE,	0x20,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1096 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngt_uqsd",	ssecmp_64_insn,	4,	NONE,	0x1A,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1388 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpcklwd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x61,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 764 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxsw",	mmxsse2_insn,	2,	NONE,	0xEE,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 60 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmc",	onebyte_insn,	1,	NONE,	0xF5,	0,	0,	0,	0,	0,	0},
#line 805 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popcnt",	cnt_insn,	3,	NONE,	0xB8,	0,	0,	0,	CPU_SSE42,	0,	0},
#line 136 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comeqps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 939 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"seto",	setcc_insn,	1,	NONE,	0x00,	0,	0,	0,	CPU_386,	0,	0},
#line 150 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgtss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 1153 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtps2pd",	avx_cvt_xmm64_insn,	3,	NONE,	0x00,	0x5A,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1387 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpcklqdq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 351 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmulp",	farithp_insn,	3,	NONE,	0xC8,	0,	0,	0,	CPU_FPU,	0,	0},
#line 90 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovz",	cmovcc_insn,	3,	NONE,	0x04,	0,	0,	0,	CPU_686,	0,	0},
#line 868 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"push",	push_insn,	33,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 586 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pandn",	mmxsse2_insn,	2,	NONE,	0xDF,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1283 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpeqq",	ssse3_insn,	3,	NONE,	0x29,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 49 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bts",	bittest_insn,	6,	NONE,	0xAB,	0x05,	0,	0,	CPU_386,	0,	0},
#line 539 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsldup",	xmm_xmm128_insn,	2,	NONE,	0xF3,	0x12,	0,	0,	CPU_SSE3,	0,	0},
#line 340 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldpi",	twobyte_insn,	1,	NONE,	0xD9,	0xEB,	0,	0,	CPU_FPU,	0,	0},
#line 890 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"repz",	NULL,	X86_LOCKREP>>8,	0xF3,	0,	0,	0,	0,	0,	0,	0},
#line 771 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminub",	mmxsse2_insn,	2,	NONE,	0xDA,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1216 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmload",	svm_rax_insn,	2,	NONE,	0xDA,	0,	0,	0,	CPU_SVM,	0,	0},
#line 207 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugtsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1181 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubaddps",	fma_128_256_insn,	4,	NONE,	0x5E,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 314 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fdivrp",	farithp_insn,	3,	NONE,	0xF0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 123 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpsd",	cmpsd_insn,	5,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 736 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubdq",	sse5two_insn,	1,	NONE,	0x63,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 792 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhuw",	mmxsse2_insn,	2,	NONE,	0xE4,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 226 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungeps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 1420 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vzeroupper",	vzero_insn,	1,	NONE,	0xC0,	0,	0,	0,	CPU_AVX,	0,	0},
#line 337 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldl2t",	twobyte_insn,	1,	NONE,	0xD9,	0xE9,	0,	0,	CPU_FPU,	0,	0},
#line 512 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movaps",	movau_insn,	6,	NONE,	0x00,	0x28,	0x01,	0,	CPU_SSE,	0,	0},
#line 358 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmsubpd",	sse5arith_insn,	4,	NONE,	0x19,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1399 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vshufps",	xmm_xmm128_imm_256_insn,	3,	NONE,	0x00,	0xC6,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1425 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xadd",	cmpxchgxadd_insn,	4,	NONE,	0xC0,	0,	0,	0,	CPU_486,	0,	0},
#line 1159 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtss2si",	cvt_rx_xmm32_insn,	4,	NONE,	0xF3,	0x2D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1130 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptrue_uspd",	ssecmp_128_insn,	3,	NONE,	0x1F,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 585 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pand",	mmxsse2_insn,	2,	NONE,	0xDB,	0,	0,	0,	CPU_MMX,	0,	0},
#line 63 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovb",	cmovcc_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_686,	0,	0},
#line 615 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomeqd",	sse5comcc_insn,	1,	NONE,	0x4E,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 526 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntdq",	movnt_insn,	1,	NONE,	0x66,	0xE7,	0,	0,	CPU_SSE2,	0,	0},
#line 379 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsetpm",	twobyte_insn,	1,	NONE,	0xDB,	0xE4,	0,	0,	CPU_286,	CPU_FPU,	CPU_Obs},
#line 1337 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxdq",	sse4m64_insn,	2,	NONE,	0x25,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 724 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddubd",	sse5two_insn,	1,	NONE,	0x52,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1106 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlepd",	ssecmp_128_insn,	3,	NONE,	0x06,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 61 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmova",	cmovcc_insn,	3,	NONE,	0x07,	0,	0,	0,	CPU_686,	0,	0},
#line 682 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 1023 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_usps",	ssecmp_128_insn,	3,	NONE,	0x18,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1006 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vandps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x54,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1169 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdpps",	sse4imm_256_insn,	3,	NONE,	0x40,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1135 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptrueps",	ssecmp_128_insn,	3,	NONE,	0x0F,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1242 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovups",	movau_insn,	6,	NONE,	0x00,	0x10,	0x01,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 443 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jle",	jcc_insn,	9,	NONE,	0x0E,	0,	0,	0,	0,	0,	0},
#line 253 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtph2ps",	cvtph2ps_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 648 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 954 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"skinit",	skinit_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SVM,	0,	0},
#line 1087 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnge_uqps",	ssecmp_128_insn,	3,	NONE,	0x19,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 416 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"insb",	onebyte_insn,	1,	NONE,	0x6C,	0x00,	0,	0,	0,	0,	0},
#line 1362 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpslld",	vpshift_insn,	4,	NONE,	0xF2,	0x72,	0x06,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1255 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmxoff",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC4,	0,	CPU_P4,	0,	0},
#line 943 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sets",	setcc_insn,	1,	NONE,	0x08,	0,	0,	0,	CPU_386,	0,	0},
#line 231 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungtsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 436 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jc",	jcc_insn,	9,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 384 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fstcw",	fstcw_insn,	1,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1213 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vminsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 637 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgew",	sse5comcc_insn,	1,	NONE,	0x4D,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 1252 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmulsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x59,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 831 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshufb",	ssse3_insn,	3,	NONE,	0x00,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 305 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcomi",	fcom2_insn,	2,	NONE,	0xDB,	0xF0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 15 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aad",	aadm_insn,	2,	NONE,	0x01,	0,	0,	NOT_64,	0,	0,	0},
#line 898 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"roundsd",	sse4m64imm_insn,	4,	NONE,	0x0B,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 629 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalsew",	sse5comcc_insn,	1,	NONE,	0x4D,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 984 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sysenter",	twobyte_insn,	1,	NONE,	0x0F,	0x34,	0,	NOT_64,	CPU_686,	0,	0},
#line 723 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddsw",	ssse3_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 912 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"scasd",	onebyte_insn,	1,	NONE,	0xAF,	0x20,	0,	0,	CPU_386,	0,	0},
#line 258 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtps2ph",	cvtps2ph_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1080 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_ussd",	ssecmp_64_insn,	4,	NONE,	0x14,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1140 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunord_ssd",	ssecmp_64_insn,	4,	NONE,	0x13,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 194 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comtrueps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0F,	0,	0,	CPU_SSE5,	0,	0},
#line 494 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lss",	lfgss_insn,	2,	NONE,	0xB2,	0,	0,	0,	CPU_386,	0,	0},
#line 503 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mfence",	threebyte_insn,	1,	NONE,	0x0F,	0xAE,	0xF0,	0,	CPU_P3,	0,	0},
#line 1123 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpordps",	ssecmp_128_insn,	3,	NONE,	0x07,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 39 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"blendps",	sse4imm_insn,	2,	NONE,	0x0C,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 772 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminud",	sse4_insn,	2,	NONE,	0x3B,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1423 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"wrmsr",	twobyte_insn,	1,	NONE,	0x0F,	0x30,	0,	0,	CPU_586,	CPU_Priv,	0},
#line 326 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"finit",	threebyte_insn,	1,	NONE,	0x9B,	0xDB,	0xE3,	0,	CPU_FPU,	0,	0},
#line 1427 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xchg",	xchg_insn,	16,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 154 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comleps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 193 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comtruepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0F,	0,	0,	CPU_SSE5,	0,	0},
#line 1158 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtss2sd",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5A,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 608 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpgtq",	sse4_insn,	2,	NONE,	0x37,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 146 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 1185 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubss",	fma_128_m32_insn,	3,	NONE,	0x6E,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 319 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ficom",	fiarith_insn,	2,	NONE,	0x02,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 1386 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpckldq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x62,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 357 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmaddss",	sse5arith32_insn,	8,	NONE,	0x12,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1021 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_uqss",	ssecmp_32_insn,	4,	NONE,	0x08,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 89 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovs",	cmovcc_insn,	3,	NONE,	0x08,	0,	0,	0,	CPU_686,	0,	0},
#line 331 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fisubr",	fiarith_insn,	2,	NONE,	0x05,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 579 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddsiw",	cyrixmmx_insn,	1,	NONE,	0x51,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 855 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubusb",	mmxsse2_insn,	2,	NONE,	0xD8,	0,	0,	0,	CPU_MMX,	0,	0},
#line 338 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldlg2",	twobyte_insn,	1,	NONE,	0xD9,	0xEC,	0,	0,	CPU_FPU,	0,	0},
#line 1293 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vperm2f128",	vperm2f128_insn,	1,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1219 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovaps",	movau_insn,	6,	NONE,	0x00,	0x28,	0x01,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 525 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movmskps",	movmsk_insn,	4,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_SSE,	0},
#line 833 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshufhw",	xmm_xmm128_imm_insn,	1,	NONE,	0xF3,	0x70,	0,	0,	CPU_SSE2,	0,	0},
#line 634 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 603 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpeqw",	mmxsse2_insn,	2,	NONE,	0x75,	0,	0,	0,	CPU_MMX,	0,	0},
#line 410 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"hsubps",	xmm_xmm128_insn,	2,	NONE,	0xF2,	0x7D,	0,	0,	CPU_SSE3,	0,	0},
#line 1258 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vorps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x56,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 475 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lfs",	lfgss_insn,	2,	NONE,	0xB4,	0,	0,	0,	CPU_386,	0,	0},
#line 1020 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_uqsd",	ssecmp_64_insn,	4,	NONE,	0x08,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 371 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frczpd",	sse5two_insn,	1,	NONE,	0x11,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1180 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubaddpd",	fma_128_256_insn,	4,	NONE,	0x5F,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 919 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setc",	setcc_insn,	1,	NONE,	0x02,	0,	0,	0,	CPU_386,	0,	0},
#line 111 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnless",	ssecmp_32_insn,	4,	NONE,	0x06,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 740 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pi2fd",	now3d_insn,	1,	NONE,	0x0D,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 811 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pperm",	sse5arith_insn,	4,	NONE,	0x23,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 571 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"packssdw",	mmxsse2_insn,	2,	NONE,	0x6B,	0,	0,	0,	CPU_MMX,	0,	0},
#line 902 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsm",	twobyte_insn,	1,	NONE,	0x0F,	0xAA,	0,	0,	CPU_586,	CPU_SMM,	0},
#line 113 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnltps",	ssecmp_128_insn,	3,	NONE,	0x05,	0,	0,	0,	CPU_SSE,	0,	0},
#line 788 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhriw",	cyrixmmx_insn,	1,	NONE,	0x5D,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 601 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpeqd",	mmxsse2_insn,	2,	NONE,	0x76,	0,	0,	0,	CPU_MMX,	0,	0},
#line 524 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movmskpd",	movmsk_insn,	4,	NONE,	0x66,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 313 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fdivr",	farith_insn,	7,	NONE,	0xF0,	0xF8,	0x07,	0,	CPU_FPU,	0,	0},
#line 464 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"js",	jcc_insn,	9,	NONE,	0x08,	0,	0,	0,	0,	0,	0},
#line 84 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnz",	cmovcc_insn,	3,	NONE,	0x05,	0,	0,	0,	CPU_686,	0,	0},
#line 263 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtsi2ss",	cvt_xmm_rmx_insn,	6,	NONE,	0xF3,	0x2A,	0,	0,	CPU_386,	CPU_SSE,	0},
#line 738 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubw",	ssse3_insn,	3,	NONE,	0x05,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 1282 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpeqd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x76,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1294 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermil2pd",	vpermil2_insn,	4,	NONE,	0x49,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 200 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comueqss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x08,	0,	0,	CPU_SSE5,	0,	0},
#line 964 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stc",	onebyte_insn,	1,	NONE,	0xF9,	0,	0,	0,	0,	0,	0},
#line 770 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminsw",	mmxsse2_insn,	2,	NONE,	0xEA,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 499 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maxpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5F,	0,	0,	CPU_SSE2,	0,	0},
#line 21 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x58,	0,	0,	CPU_SSE,	0,	0},
#line 392 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ftst",	twobyte_insn,	1,	NONE,	0xD9,	0xE4,	0,	0,	CPU_FPU,	0,	0},
#line 700 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pf2iw",	now3d_insn,	1,	NONE,	0x1C,	0,	0,	0,	CPU_3DNow,	CPU_Athlon,	0},
#line 440 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jg",	jcc_insn,	9,	NONE,	0x0F,	0,	0,	0,	0,	0,	0},
#line 309 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcos",	twobyte_insn,	1,	NONE,	0xD9,	0xFF,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 417 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"insd",	onebyte_insn,	1,	NONE,	0x6D,	0x20,	0,	0,	CPU_386,	0,	0},
#line 614 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomeqb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 141 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comfalsesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0B,	0,	0,	CPU_SSE5,	0,	0},
#line 492 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loopz",	loop_insn,	8,	NONE,	0x01,	0,	0,	0,	0,	0,	0},
#line 605 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpestrm",	sse4pcmpstr_insn,	1,	NONE,	0x60,	0,	0,	0,	CPU_SSE42,	0,	0},
#line 1027 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeqps",	ssecmp_128_insn,	3,	NONE,	0x00,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 974 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sub",	arith_insn,	22,	NONE,	0x28,	0x05,	0,	0,	0,	0,	0},
#line 1053 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgtss",	ssecmp_32_insn,	4,	NONE,	0x0E,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 365 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnstenv",	onebytemem_insn,	1,	NONE,	0x06,	0xD9,	0,	0,	CPU_FPU,	0,	0},
#line 457 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jns",	jcc_insn,	9,	NONE,	0x09,	0,	0,	0,	0,	0,	0},
#line 1300 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilpd",	vpermil_insn,	4,	NONE,	0x05,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1122 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpordpd",	ssecmp_128_insn,	3,	NONE,	0x07,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 960 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sqrtpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x51,	0,	0,	CPU_SSE2,	0,	0},
#line 463 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jrcxz",	jcxz_insn,	2,	NONE,	0x40,	0,	0,	ONLY_64,	0,	0,	0},
#line 929 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnc",	setcc_insn,	1,	NONE,	0x03,	0,	0,	0,	CPU_386,	0,	0},
#line 624 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1319 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaddubsw",	ssse3_insn,	3,	NONE,	0x04,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1363 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpslldq",	pslrldq_insn,	2,	NONE,	0x07,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 725 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddubq",	sse5two_insn,	1,	NONE,	0x53,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1264 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpackusdw",	ssse3_insn,	3,	NONE,	0x2B,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 850 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubd",	mmxsse2_insn,	2,	NONE,	0xFA,	0,	0,	0,	CPU_MMX,	0,	0},
#line 619 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomequq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 48 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"btr",	bittest_insn,	6,	NONE,	0xB3,	0x06,	0,	0,	CPU_386,	0,	0},
#line 757 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmadcsswd",	sse5pmacs_insn,	1,	NONE,	0xA6,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 426 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"invlpg",	twobytemem_insn,	1,	NONE,	0x07,	0x0F,	0x01,	0,	CPU_486,	CPU_Priv,	0},
#line 676 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 845 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psrld",	pshift_insn,	4,	NONE,	0xD2,	0x72,	0x02,	0,	CPU_MMX,	0,	0},
#line 715 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfrsqrt",	now3d_insn,	1,	NONE,	0x97,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 1129 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpss",	xmm_xmm32_imm_insn,	4,	NONE,	0xF3,	0xC2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 702 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfadd",	now3d_insn,	1,	NONE,	0x9E,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 378 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fscale",	twobyte_insn,	1,	NONE,	0xD9,	0xFD,	0,	0,	CPU_FPU,	0,	0},
#line 134 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpxchg8b",	cmpxchg8b_insn,	1,	NONE,	0,	0,	0,	0,	CPU_586,	0,	0},
#line 1103 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnle_uqps",	ssecmp_128_insn,	3,	NONE,	0x16,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1041 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpge_oqss",	ssecmp_32_insn,	4,	NONE,	0x1D,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 486 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lodsq",	onebyte_insn,	1,	NONE,	0xAD,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 355 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmaddps",	sse5arith_insn,	4,	NONE,	0x10,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1257 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vorpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x56,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 814 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetcht0",	twobytemem_insn,	1,	NONE,	0x01,	0x0F,	0x18,	0,	CPU_P3,	0,	0},
#line 687 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomub",	sse5com_insn,	1,	NONE,	0x6C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 780 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxwq",	sse4m32_insn,	2,	NONE,	0x24,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1085 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneqss",	ssecmp_32_insn,	4,	NONE,	0x04,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 455 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jno",	jcc_insn,	9,	NONE,	0x01,	0,	0,	0,	0,	0,	0},
#line 1075 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_osps",	ssecmp_128_insn,	3,	NONE,	0x1C,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 638 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1205 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaxpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x5F,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1071 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_oqps",	ssecmp_128_insn,	3,	NONE,	0x0C,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1325 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxud",	ssse3_insn,	3,	NONE,	0x3F,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 103 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpltss",	ssecmp_32_insn,	4,	NONE,	0x01,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 82 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnp",	cmovcc_insn,	3,	NONE,	0x0B,	0,	0,	0,	CPU_686,	0,	0},
#line 947 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shl",	shift_insn,	16,	NONE,	0x04,	0,	0,	0,	0,	0,	0},
#line 1377 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubusb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xD8,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 209 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comulepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 994 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"unpckhps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x15,	0,	0,	CPU_SSE,	0,	0},
#line 230 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungtps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 819 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"protd",	sse5prot_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1209 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmcall",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC1,	0,	CPU_P4,	0,	0},
#line 1222 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovdqa",	movau_insn,	6,	NONE,	0x66,	0x6F,	0x10,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1145 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunordss",	ssecmp_32_insn,	4,	NONE,	0x03,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 583 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddw",	mmxsse2_insn,	2,	NONE,	0xFD,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1289 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpgtq",	ssse3_insn,	3,	NONE,	0x37,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1101 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngtss",	ssecmp_32_insn,	4,	NONE,	0x0A,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1078 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_uspd",	ssecmp_128_insn,	3,	NONE,	0x14,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 769 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminsd",	sse4_insn,	2,	NONE,	0x39,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 733 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phminposuw",	sse4_insn,	2,	NONE,	0x41,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 73 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnb",	cmovcc_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_686,	0,	0},
#line 689 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomuq",	sse5com_insn,	1,	NONE,	0x6F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1118 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpord_spd",	ssecmp_128_insn,	3,	NONE,	0x17,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1331 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminud",	ssse3_insn,	3,	NONE,	0x3B,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 955 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sldt",	sldtmsw_insn,	6,	NONE,	0x00,	0x00,	0,	0,	CPU_286,	0,	0},
#line 892 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"retf",	retnf_insn,	6,	NONE,	0xCA,	0x40,	0,	0,	0,	0,	0},
#line 797 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmvgezb",	cyrixmmx_insn,	1,	NONE,	0x5C,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 68 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovge",	cmovcc_insn,	3,	NONE,	0x0D,	0,	0,	0,	CPU_686,	0,	0},
#line 647 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomled",	sse5comcc_insn,	1,	NONE,	0x4E,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 466 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lahf",	onebyte_insn,	1,	NONE,	0x9F,	0,	0,	0,	0,	0,	0},
#line 1137 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptruess",	ssecmp_32_insn,	4,	NONE,	0x0F,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 151 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comisd",	xmm_xmm64_insn,	4,	NONE,	0x66,	0x2F,	0,	0,	CPU_SSE2,	0,	0},
#line 704 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfcmpge",	now3d_insn,	1,	NONE,	0x90,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 51 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cbw",	onebyte_insn,	1,	NONE,	0x98,	0x10,	0,	0,	0,	0,	0},
#line 1426 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xbts",	xbts_insn,	2,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_Obs,	CPU_Undoc},
#line 670 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomnequq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 581 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddusb",	mmxsse2_insn,	2,	NONE,	0xDC,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1411 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vucomisd",	avx_xmm_xmm64_insn,	2,	NONE,	0x66,	0x2E,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 483 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lock",	NULL,	X86_LOCKREP>>8,	0xF0,	0,	0,	0,	0,	0,	0,	0},
#line 38 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"blendpd",	sse4imm_insn,	2,	NONE,	0x0D,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 899 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"roundss",	sse4m32imm_insn,	4,	NONE,	0x0A,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 501 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maxsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5F,	0,	0,	CPU_SSE2,	0,	0},
#line 244 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunordss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 429 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"iretd",	onebyte_insn,	1,	NONE,	0xCF,	0x20,	0,	0,	CPU_386,	0,	0},
#line 153 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comlepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1254 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmwrite",	vmxmemwr_insn,	2,	NONE,	0,	0,	0,	0,	CPU_P4,	0,	0},
#line 737 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubsw",	ssse3_insn,	3,	NONE,	0x07,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 707 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfmin",	now3d_insn,	1,	NONE,	0x94,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 272 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cwd",	onebyte_insn,	1,	NONE,	0x99,	0x10,	0,	0,	0,	0,	0},
#line 1359 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsignb",	ssse3_insn,	3,	NONE,	0x08,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 795 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmullw",	mmxsse2_insn,	2,	NONE,	0xD5,	0,	0,	0,	CPU_MMX,	0,	0},
#line 487 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lodsw",	onebyte_insn,	1,	NONE,	0xAD,	0x10,	0,	0,	0,	0,	0},
#line 1097 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngt_uqss",	ssecmp_32_insn,	4,	NONE,	0x1A,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 254 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtpi2pd",	cvt_xmm_mm_ss_insn,	1,	NONE,	0x66,	0x2A,	0,	0,	CPU_SSE2,	0,	0},
#line 654 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1372 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xF8,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 415 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"inc",	incdec_insn,	6,	NONE,	0x40,	0x00,	0,	0,	0,	0,	0},
#line 1321 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxsb",	ssse3_insn,	3,	NONE,	0x3C,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1161 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvttps2dq",	avx_xmm_xmm128_insn,	2,	NONE,	0xF3,	0x5B,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 423 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"int3",	onebyte_insn,	1,	NONE,	0xCC,	0,	0,	0,	0,	0,	0},
#line 933 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnl",	setcc_insn,	1,	NONE,	0x0D,	0,	0,	0,	CPU_386,	0,	0},
#line 993 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"unpckhpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x15,	0,	0,	CPU_SSE2,	0,	0},
#line 212 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuless",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 815 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetcht1",	twobytemem_insn,	1,	NONE,	0x02,	0x0F,	0x18,	0,	CPU_P3,	0,	0},
#line 1326 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxuw",	ssse3_insn,	3,	NONE,	0x3E,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1014 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_ospd",	ssecmp_128_insn,	3,	NONE,	0x10,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 277 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"div",	div_insn,	8,	NONE,	0x06,	0,	0,	0,	0,	0,	0},
#line 709 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfnacc",	now3d_insn,	1,	NONE,	0x8A,	0,	0,	0,	CPU_3DNow,	CPU_Athlon,	0},
#line 17 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aas",	onebyte_insn,	1,	NONE,	0x3F,	0,	0,	NOT_64,	0,	0,	0},
#line 144 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgeps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 140 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comfalseps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0B,	0,	0,	CPU_SSE5,	0,	0},
#line 840 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pslldq",	pslrldq_insn,	2,	NONE,	0x07,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 1206 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaxps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x5F,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 965 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"std",	onebyte_insn,	1,	NONE,	0xFD,	0,	0,	0,	0,	0,	0},
#line 572 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"packsswb",	mmxsse2_insn,	2,	NONE,	0x63,	0,	0,	0,	CPU_MMX,	0,	0},
#line 538 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movshdup",	xmm_xmm128_insn,	2,	NONE,	0xF3,	0x16,	0,	0,	CPU_SSE3,	0,	0},
#line 56 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"clflush",	clflush_insn,	1,	NONE,	0,	0,	0,	0,	CPU_P3,	0,	0},
#line 375 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frndint",	twobyte_insn,	1,	NONE,	0xD9,	0xFC,	0,	0,	CPU_FPU,	0,	0},
#line 293 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fbstp",	fbldstp_insn,	1,	NONE,	0x06,	0,	0,	0,	CPU_FPU,	0,	0},
#line 529 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntpd",	movnt_insn,	1,	NONE,	0x66,	0x2B,	0,	0,	CPU_SSE2,	0,	0},
#line 551 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mulps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x59,	0,	0,	CPU_SSE,	0,	0},
#line 302 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovnu",	fcmovcc_insn,	1,	NONE,	0xDB,	0xD8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 419 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"insertq",	insertq_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SSE4a,	0,	0},
#line 405 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fyl2xp1",	twobyte_insn,	1,	NONE,	0xD9,	0xF9,	0,	0,	CPU_FPU,	0,	0},
#line 1210 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmclear",	vmxthreebytemem_insn,	1,	NONE,	0x66,	0,	0,	0,	CPU_P4,	0,	0},
#line 1062 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplt_oqpd",	ssecmp_128_insn,	3,	NONE,	0x11,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 556 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"nop",	onebyte_insn,	1,	NONE,	0x90,	0,	0,	0,	0,	0,	0},
#line 1186 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmaddpd",	fma_128_256_insn,	4,	NONE,	0x79,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 528 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movnti",	movnti_insn,	2,	NONE,	0,	0,	0,	0,	CPU_P4,	0,	0},
#line 613 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomd",	sse5com_insn,	1,	NONE,	0x4E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 907 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sal",	shift_insn,	16,	NONE,	0x04,	0,	0,	0,	0,	0,	0},
#line 1341 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxbq",	sse4m16_insn,	2,	NONE,	0x32,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1333 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovmskb",	pmovmskb_insn,	4,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1339 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxwq",	sse4m32_insn,	2,	NONE,	0x24,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 555 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"neg",	f6_insn,	4,	NONE,	0x03,	0,	0,	0,	0,	0,	0},
#line 441 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jge",	jcc_insn,	9,	NONE,	0x0D,	0,	0,	0,	0,	0,	0},
#line 1225 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovhpd",	movhlp_insn,	3,	NONE,	0x66,	0x16,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1188 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmaddsd",	fma_128_m64_insn,	3,	NONE,	0x7B,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1431 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xcryptecb",	padlock_insn,	1,	NONE,	0xC8,	0xF3,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 196 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comtruess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0F,	0,	0,	CPU_SSE5,	0,	0},
#line 458 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnz",	jcc_insn,	9,	NONE,	0x05,	0,	0,	0,	0,	0,	0},
#line 65 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovc",	cmovcc_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_686,	0,	0},
#line 167 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 310 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fdecstp",	twobyte_insn,	1,	NONE,	0xD9,	0xF6,	0,	0,	CPU_FPU,	0,	0},
#line 1060 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplesd",	ssecmp_64_insn,	4,	NONE,	0x02,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 72 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnae",	cmovcc_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_686,	0,	0},
#line 531 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntq",	movntq_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1392 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vroundpd",	avx_sse4imm_insn,	3,	NONE,	0x09,	0,	0,	ONLY_AVX,	CPU_SSE41,	0,	0},
#line 267 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttpd2pi",	cvt_mm_xmm_insn,	1,	NONE,	0x66,	0x2C,	0,	0,	CPU_SSE2,	0,	0},
#line 1302 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermiltd2pd",	vpermil2_fixed_insn,	4,	NONE,	0x49,	0x00,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1022 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_uspd",	ssecmp_128_insn,	3,	NONE,	0x18,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 59 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"clts",	twobyte_insn,	1,	NONE,	0x0F,	0x06,	0,	0,	CPU_286,	CPU_Priv,	0},
#line 1397 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vrsqrtss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x52,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 118 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpordsd",	ssecmp_64_insn,	4,	NONE,	0x07,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 352 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnclex",	twobyte_insn,	1,	NONE,	0xDB,	0xE2,	0,	0,	CPU_FPU,	0,	0},
#line 1389 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpxor",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xEF,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 50 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"call",	call_insn,	26,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1136 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptruesd",	ssecmp_64_insn,	4,	NONE,	0x0F,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1253 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmulss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x59,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1055 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmple_oqps",	ssecmp_128_insn,	3,	NONE,	0x12,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1197 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vhsubps",	xmm_xmm128_256_insn,	3,	NONE,	0xF2,	0x7D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1164 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdivpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x5E,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1381 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpckhbw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x68,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 621 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomeqw",	sse5comcc_insn,	1,	NONE,	0x4D,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 1166 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdivsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5E,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1230 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovmskpd",	movmsk_insn,	4,	NONE,	0x66,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 838 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psignw",	ssse3_insn,	3,	NONE,	0x09,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 785 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxwd",	sse4m64_insn,	2,	NONE,	0x33,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 279 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"divps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x5E,	0,	0,	CPU_SSE,	0,	0},
#line 502 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maxss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5F,	0,	0,	CPU_SSE,	0,	0},
#line 1297 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilmo2ps",	vpermil2_fixed_insn,	4,	NONE,	0x48,	0x02,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 108 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnlepd",	ssecmp_128_insn,	3,	NONE,	0x06,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 343 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmaddps",	sse5arith_insn,	4,	NONE,	0x00,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 844 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psraw",	pshift_insn,	4,	NONE,	0xE1,	0x71,	0x04,	0,	CPU_MMX,	0,	0},
#line 587 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pause",	onebyte_prefix_insn,	1,	NONE,	0xF3,	0x90,	0,	0,	CPU_P4,	0,	0},
#line 216 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comultss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 148 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgtps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 178 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnleps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 372 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frczps",	sse5two_insn,	1,	NONE,	0x10,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 296 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovb",	fcmovcc_insn,	1,	NONE,	0xDA,	0xC0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 98 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmplesd",	ssecmp_64_insn,	4,	NONE,	0x02,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 1144 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunordsd",	ssecmp_64_insn,	4,	NONE,	0x03,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 910 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sbb",	arith_insn,	22,	NONE,	0x18,	0x03,	0,	0,	0,	0,	0},
#line 864 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpcklbw",	mmxsse2_insn,	2,	NONE,	0x60,	0,	0,	0,	CPU_MMX,	0,	0},
#line 208 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugtss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 445 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jna",	jcc_insn,	9,	NONE,	0x06,	0,	0,	0,	0,	0,	0},
#line 1403 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsqrtss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x51,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 598 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pclmulqdq",	aes_imm_insn,	1,	NONE,	0x3A,	0x44,	0,	0,	CPU_CLMUL,	0,	0},
#line 711 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfrcp",	now3d_insn,	1,	NONE,	0x96,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 1202 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaskmovdqu",	maskmovdqu_insn,	1,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1124 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpordsd",	ssecmp_64_insn,	4,	NONE,	0x07,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1305 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpextrd",	pextrd_insn,	1,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 841 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psllq",	pshift_insn,	4,	NONE,	0xF3,	0x73,	0x06,	0,	CPU_MMX,	0,	0},
#line 801 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pop",	pop_insn,	21,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1349 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmulhw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE5,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 399 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fxam",	twobyte_insn,	1,	NONE,	0xD9,	0xE5,	0,	0,	CPU_FPU,	0,	0},
#line 552 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mulsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x59,	0,	0,	CPU_SSE2,	0,	0},
#line 1270 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddsw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xED,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 664 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 717 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfsubr",	now3d_insn,	1,	NONE,	0xAA,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 599 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmov",	sse5arith_insn,	4,	NONE,	0x22,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 948 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shld",	shlrd_insn,	9,	NONE,	0xA4,	0,	0,	0,	CPU_386,	0,	0},
#line 1408 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsubss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 24 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addsubpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xD0,	0,	0,	CPU_SSE3,	0,	0},
#line 1138 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunord_spd",	ssecmp_128_insn,	3,	NONE,	0x13,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 997 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x58,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1312 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphsubd",	ssse3_insn,	3,	NONE,	0x06,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 752 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacssdql",	sse5pmacs_insn,	1,	NONE,	0x87,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 387 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fstsw",	fstsw_insn,	2,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 128 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpunordps",	ssecmp_128_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_SSE,	0,	0},
#line 869 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pusha",	onebyte_insn,	1,	NONE,	0x60,	0x00,	0,	NOT_64,	CPU_186,	0,	0},
#line 228 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 344 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmaddsd",	sse5arith64_insn,	8,	NONE,	0x03,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 438 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"je",	jcc_insn,	9,	NONE,	0x04,	0,	0,	0,	0,	0,	0},
#line 70 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovle",	cmovcc_insn,	3,	NONE,	0x0E,	0,	0,	0,	CPU_686,	0,	0},
#line 520 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movhps",	movhlp_insn,	3,	NONE,	0x00,	0x16,	0,	0,	CPU_SSE,	0,	0},
#line 1418 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vxorps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x57,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1111 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlt_uqps",	ssecmp_128_insn,	3,	NONE,	0x15,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 936 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnp",	setcc_insn,	1,	NONE,	0x0B,	0,	0,	0,	CPU_386,	0,	0},
#line 527 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntdqa",	movntdqa_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 976 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"subps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x5C,	0,	0,	CPU_SSE,	0,	0},
#line 618 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomequd",	sse5comcc_insn,	1,	NONE,	0x6E,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 1243 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmpsadbw",	sse4imm_insn,	2,	NONE,	0x42,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1432 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xcryptofb",	padlock_insn,	1,	NONE,	0xE8,	0xF3,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 884 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rdtsc",	twobyte_insn,	1,	NONE,	0x0F,	0x31,	0,	0,	CPU_586,	0,	0},
#line 47 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"btc",	bittest_insn,	6,	NONE,	0xBB,	0x07,	0,	0,	CPU_386,	0,	0},
#line 804 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popaw",	onebyte_insn,	1,	NONE,	0x61,	0x10,	0,	NOT_64,	CPU_186,	0,	0},
#line 575 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddb",	mmxsse2_insn,	2,	NONE,	0xFC,	0,	0,	0,	CPU_MMX,	0,	0},
#line 837 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psignd",	ssse3_insn,	3,	NONE,	0x0A,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 300 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovnbe",	fcmovcc_insn,	1,	NONE,	0xDB,	0xD0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 290 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fadd",	farith_insn,	7,	NONE,	0xC0,	0xC0,	0x00,	0,	CPU_FPU,	0,	0},
#line 667 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneqq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 507 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"minss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5D,	0,	0,	CPU_SSE,	0,	0},
#line 122 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpsb",	onebyte_insn,	1,	NONE,	0xA6,	0x00,	0,	0,	0,	0,	0},
#line 166 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comneqss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 448 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnbe",	jcc_insn,	9,	NONE,	0x07,	0,	0,	0,	0,	0,	0},
#line 782 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxbq",	sse4m16_insn,	2,	NONE,	0x32,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1050 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgtpd",	ssecmp_128_insn,	3,	NONE,	0x0E,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 356 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmaddsd",	sse5arith64_insn,	8,	NONE,	0x13,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 988 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ucomisd",	xmm_xmm64_insn,	4,	NONE,	0x66,	0x2E,	0,	0,	CPU_SSE2,	0,	0},
#line 175 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngtsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 701 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfacc",	now3d_insn,	1,	NONE,	0xAE,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 30 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aesimc",	aes_insn,	1,	NONE,	0x38,	0xDB,	0,	0,	CPU_AES,	0,	0},
#line 1422 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"wbinvd",	twobyte_insn,	1,	NONE,	0x0F,	0x09,	0,	0,	CPU_486,	CPU_Priv,	0},
#line 530 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntps",	movnt_insn,	1,	NONE,	0x00,	0x2B,	0,	0,	CPU_SSE,	0,	0},
#line 307 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcomp",	fcom_insn,	5,	NONE,	0xD8,	0x03,	0,	0,	CPU_FPU,	0,	0},
#line 397 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fucompp",	twobyte_insn,	1,	NONE,	0xDA,	0xE9,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 246 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cqo",	onebyte_insn,	1,	NONE,	0x99,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 1336 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxbw",	sse4m64_insn,	2,	NONE,	0x20,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 820 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"protq",	sse5prot_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1070 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_oqpd",	ssecmp_128_insn,	3,	NONE,	0x0C,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1125 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpordss",	ssecmp_32_insn,	4,	NONE,	0x07,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1063 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplt_oqps",	ssecmp_128_insn,	3,	NONE,	0x11,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 779 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxwd",	sse4m64_insn,	2,	NONE,	0x23,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 294 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fchs",	twobyte_insn,	1,	NONE,	0xD9,	0xE0,	0,	0,	CPU_FPU,	0,	0},
#line 763 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxsd",	sse4_insn,	2,	NONE,	0x3D,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 916 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setae",	setcc_insn,	1,	NONE,	0x03,	0,	0,	0,	CPU_386,	0,	0},
#line 1385 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpcklbw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x60,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 318 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fiadd",	fiarith_insn,	2,	NONE,	0x00,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 987 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"test",	test_insn,	20,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 827 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshlb",	sse5psh_insn,	2,	NONE,	0x00,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 570 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pabsw",	ssse3_insn,	3,	NONE,	0x1D,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 1348 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmulhuw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE4,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 564 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"out",	out_insn,	12,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 908 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"salc",	onebyte_insn,	1,	NONE,	0xD6,	0,	0,	NOT_64,	CPU_Undoc,	0,	0},
#line 1224 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovhlps",	movhllhps_insn,	2,	NONE,	0x12,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1233 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovntdqa",	movntdqa_insn,	1,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 400 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fxch",	fxch_insn,	4,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1007 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vblendpd",	sse4imm_256_insn,	3,	NONE,	0x0D,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 913 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"scasq",	onebyte_insn,	1,	NONE,	0xAF,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 829 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshlq",	sse5psh_insn,	2,	NONE,	0x03,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 364 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnstcw",	fldnstcw_insn,	1,	NONE,	0x07,	0,	0,	0,	CPU_FPU,	0,	0},
#line 195 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comtruesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0F,	0,	0,	CPU_SSE5,	0,	0},
#line 768 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminsb",	sse4_insn,	2,	NONE,	0x38,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 506 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"minsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5D,	0,	0,	CPU_SSE2,	0,	0},
#line 199 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comueqsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x08,	0,	0,	CPU_SSE5,	0,	0},
#line 669 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomnequd",	sse5comcc_insn,	1,	NONE,	0x6E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 743 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pinsrd",	pinsrd_insn,	2,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_SSE41,	0},
#line 1261 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpabsw",	avx_ssse3_2op_insn,	1,	NONE,	0x1D,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 839 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pslld",	pshift_insn,	4,	NONE,	0xF2,	0x72,	0x06,	0,	CPU_MMX,	0,	0},
#line 298 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmove",	fcmovcc_insn,	1,	NONE,	0xDA,	0xC8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 998 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x58,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 360 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmsubsd",	sse5arith64_insn,	8,	NONE,	0x1B,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 328 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fistp",	fildstp_insn,	3,	NONE,	0x03,	0x02,	0x07,	0,	CPU_FPU,	0,	0},
#line 242 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunordps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 557 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"not",	f6_insn,	4,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 1160 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvttpd2dq",	avx_cvt_xmm128_insn,	2,	NONE,	0x66,	0xE6,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 137 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comeqsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 19 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"add",	arith_insn,	22,	NONE,	0x00,	0x00,	0,	0,	0,	0,	0},
#line 1056 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmple_oqsd",	ssecmp_64_insn,	4,	NONE,	0x12,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 262 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtsi2sd",	cvt_xmm_rmx_insn,	6,	NONE,	0xF2,	0x2A,	0,	0,	CPU_SSE2,	0,	0},
#line 93 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpeqps",	ssecmp_128_insn,	3,	NONE,	0x00,	0,	0,	0,	CPU_SSE,	0,	0},
#line 962 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sqrtsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x51,	0,	0,	CPU_SSE2,	0,	0},
#line 1435 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xor",	arith_insn,	22,	NONE,	0x30,	0x06,	0,	0,	0,	0,	0},
#line 617 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomequb",	sse5comcc_insn,	1,	NONE,	0x6C,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 112 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnltpd",	ssecmp_128_insn,	3,	NONE,	0x05,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 390 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsubr",	farith_insn,	7,	NONE,	0xE0,	0xE8,	0x05,	0,	CPU_FPU,	0,	0},
#line 287 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"extrq",	extrq_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SSE4a,	0,	0},
#line 781 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxbd",	sse4m32_insn,	2,	NONE,	0x31,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1280 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpblendw",	sse4imm_insn,	2,	NONE,	0x0E,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1134 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptruepd",	ssecmp_128_insn,	3,	NONE,	0x0F,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1307 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpextrw",	pextrw_insn,	7,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 453 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnl",	jcc_insn,	9,	NONE,	0x0D,	0,	0,	0,	0,	0,	0},
#line 1285 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpestri",	sse4pcmpstr_insn,	1,	NONE,	0x61,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1012 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vbroadcastsd",	vbroadcastsd_insn,	1,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 674 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1409 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vtestpd",	sse4_insn,	2,	NONE,	0x0F,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 14 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aaa",	onebyte_insn,	1,	NONE,	0x37,	0,	0,	NOT_64,	0,	0,	0},
#line 886 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rep",	NULL,	X86_LOCKREP>>8,	0xF3,	0,	0,	0,	0,	0,	0,	0},
#line 1174 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddpd",	fma_128_256_insn,	4,	NONE,	0x69,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 191 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comsd",	sse5com64_insn,	2,	NONE,	0x2F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 917 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setb",	setcc_insn,	1,	NONE,	0x02,	0,	0,	0,	CPU_386,	0,	0},
#line 12 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"a32",	NULL,	X86_ADDRSIZE>>8,	0x20,	0,	0,	0,	0,	0,	0,	0},
#line 695 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pextrb",	pextrb_insn,	3,	NONE,	0,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1428 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xcryptcbc",	padlock_insn,	1,	NONE,	0xD0,	0xF3,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 1132 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptrue_ussd",	ssecmp_64_insn,	4,	NONE,	0x1F,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 96 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmplepd",	ssecmp_128_insn,	3,	NONE,	0x02,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 546 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movups",	movau_insn,	6,	NONE,	0x00,	0x10,	0x01,	0,	CPU_SSE,	0,	0},
#line 404 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fyl2x",	twobyte_insn,	1,	NONE,	0xD9,	0xF1,	0,	0,	CPU_FPU,	0,	0},
#line 1072 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_oqsd",	ssecmp_64_insn,	4,	NONE,	0x0C,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 548 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mpsadbw",	sse4imm_insn,	2,	NONE,	0x42,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 476 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lgdt",	twobytemem_insn,	1,	NONE,	0x02,	0x0F,	0x01,	0,	CPU_286,	CPU_Priv,	0},
#line 221 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuneqsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 260 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtsd2si",	cvt_rx_xmm64_insn,	4,	NONE,	0xF2,	0x2D,	0,	0,	CPU_386,	CPU_SSE2,	0},
#line 78 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnge",	cmovcc_insn,	3,	NONE,	0x0C,	0,	0,	0,	CPU_686,	0,	0},
#line 119 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpordss",	ssecmp_32_insn,	4,	NONE,	0x07,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 247 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"crc32",	crc32_insn,	5,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_SSE42,	0},
#line 1371 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsrlw",	vpshift_insn,	4,	NONE,	0xD1,	0x71,	0x02,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 569 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pabsd",	ssse3_insn,	3,	NONE,	0x1E,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 784 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovzxdq",	sse4m64_insn,	2,	NONE,	0x35,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 281 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"divss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5E,	0,	0,	CPU_SSE,	0,	0},
#line 97 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpleps",	ssecmp_128_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1126 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmppd",	xmm_xmm128_imm_256_insn,	3,	NONE,	0x66,	0xC2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 366 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnstsw",	fnstsw_insn,	2,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1127 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpps",	xmm_xmm128_imm_256_insn,	3,	NONE,	0x00,	0xC2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1089 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnge_uqss",	ssecmp_32_insn,	4,	NONE,	0x19,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1009 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vblendvpd",	avx_sse4xmm0_insn,	2,	NONE,	0x4B,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 350 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmul",	farith_insn,	7,	NONE,	0xC8,	0xC8,	0x01,	0,	CPU_FPU,	0,	0},
#line 1152 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtps2dq",	avx_xmm_xmm128_insn,	2,	NONE,	0x66,	0x5B,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1274 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpalignr",	sse4imm_insn,	2,	NONE,	0x0F,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1410 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vtestps",	sse4_insn,	2,	NONE,	0x0E,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 924 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setle",	setcc_insn,	1,	NONE,	0x0E,	0,	0,	0,	CPU_386,	0,	0},
#line 80 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnle",	cmovcc_insn,	3,	NONE,	0x0F,	0,	0,	0,	CPU_686,	0,	0},
#line 921 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setg",	setcc_insn,	1,	NONE,	0x0F,	0,	0,	0,	CPU_386,	0,	0},
#line 1402 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsqrtsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x51,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1310 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphaddw",	ssse3_insn,	3,	NONE,	0x01,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1376 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubsw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE9,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1347 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmulhrsw",	ssse3_insn,	3,	NONE,	0x0B,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1054 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmple_oqpd",	ssecmp_128_insn,	3,	NONE,	0x12,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 882 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rdpmc",	twobyte_insn,	1,	NONE,	0x0F,	0x33,	0,	0,	CPU_686,	0,	0},
#line 160 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comltss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 1026 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeqpd",	ssecmp_128_insn,	3,	NONE,	0x00,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 446 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnae",	jcc_insn,	9,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 66 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmove",	cmovcc_insn,	3,	NONE,	0x04,	0,	0,	0,	CPU_686,	0,	0},
#line 1178 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddsubpd",	fma_128_256_insn,	4,	NONE,	0x5D,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1380 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vptest",	sse4_insn,	2,	NONE,	0x17,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 20 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x58,	0,	0,	CPU_SSE2,	0,	0},
#line 774 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovmskb",	pmovmskb_insn,	4,	NONE,	0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 523 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movlps",	movhlp_insn,	3,	NONE,	0x00,	0x12,	0,	0,	CPU_SSE,	0,	0},
#line 268 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttps2dq",	xmm_xmm128_insn,	2,	NONE,	0xF3,	0x5B,	0,	0,	CPU_SSE2,	0,	0},
#line 335 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldenv",	onebytemem_insn,	1,	NONE,	0x04,	0xD9,	0,	0,	CPU_FPU,	0,	0},
#line 125 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpss",	xmm_xmm32_imm_insn,	4,	NONE,	0xF3,	0xC2,	0,	0,	CPU_SSE,	0,	0},
#line 412 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"idiv",	div_insn,	8,	NONE,	0x07,	0,	0,	0,	0,	0,	0},
#line 1391 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vrcpss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x53,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 777 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxbw",	sse4m64_insn,	2,	NONE,	0x20,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1298 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilmz2pd",	vpermil2_fixed_insn,	4,	NONE,	0x49,	0x03,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 726 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddubw",	sse5two_insn,	1,	NONE,	0x51,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 303 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovu",	fcmovcc_insn,	1,	NONE,	0xDA,	0xD8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 990 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ud1",	twobyte_insn,	1,	NONE,	0x0F,	0xB9,	0,	0,	CPU_286,	CPU_Undoc,	0},
#line 516 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movdqa",	movau_insn,	6,	NONE,	0x66,	0x6F,	0x10,	0,	CPU_SSE2,	0,	0},
#line 1043 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgeps",	ssecmp_128_insn,	3,	NONE,	0x0D,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 683 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 249 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtdq2ps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x5B,	0,	0,	CPU_SSE2,	0,	0},
#line 542 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsw",	onebyte_insn,	1,	NONE,	0xA5,	0x10,	0,	0,	0,	0,	0},
#line 1353 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpor",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xEB,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 787 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmuldq",	sse4_insn,	2,	NONE,	0x28,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1275 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpand",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDB,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 58 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cli",	onebyte_insn,	1,	NONE,	0xFA,	0,	0,	0,	0,	0,	0},
#line 181 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnltpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 284 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"emms",	twobyte_insn,	1,	NONE,	0x0F,	0x77,	0,	0,	CPU_MMX,	0,	0},
#line 1327 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminsb",	ssse3_insn,	3,	NONE,	0x38,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 174 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngtps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 1163 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvttss2si",	cvt_rx_xmm32_insn,	4,	NONE,	0xF3,	0x2C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 942 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setpo",	setcc_insn,	1,	NONE,	0x0B,	0,	0,	0,	CPU_386,	0,	0},
#line 758 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmadcswd",	sse5pmacs_insn,	1,	NONE,	0xB6,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 750 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacssdd",	sse5pmacs_insn,	1,	NONE,	0x8E,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 789 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhrsw",	ssse3_insn,	3,	NONE,	0x0B,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 1434 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xlatb",	onebyte_insn,	1,	NONE,	0xD7,	0x00,	0,	0,	0,	0,	0},
#line 953 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sidt",	twobytemem_insn,	1,	NONE,	0x01,	0x0F,	0x01,	0,	CPU_286,	CPU_Priv,	0},
#line 696 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pextrd",	pextrd_insn,	1,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_SSE41,	0},
#line 1322 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaxsd",	ssse3_insn,	3,	NONE,	0x3D,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 798 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmvlzb",	cyrixmmx_insn,	1,	NONE,	0x5B,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 1143 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunordps",	ssecmp_128_insn,	3,	NONE,	0x03,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1342 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxbw",	sse4m64_insn,	2,	NONE,	0x30,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1407 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsubsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 656 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 858 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pswapd",	now3d_insn,	1,	NONE,	0xBB,	0,	0,	0,	CPU_3DNow,	CPU_Athlon,	0},
#line 1077 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_osss",	ssecmp_32_insn,	4,	NONE,	0x1C,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 342 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmaddpd",	sse5arith_insn,	4,	NONE,	0x01,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1150 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtpd2dq",	avx_cvt_xmm128_insn,	2,	NONE,	0xF2,	0xE6,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1094 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngt_uqpd",	ssecmp_128_insn,	3,	NONE,	0x1A,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 628 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1442 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xsha256",	padlock_insn,	1,	NONE,	0xD0,	0xF3,	0xA6,	0,	CPU_PadLock,	0,	0},
#line 830 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshlw",	sse5psh_insn,	2,	NONE,	0x01,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1309 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphaddsw",	ssse3_insn,	3,	NONE,	0x03,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 115 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnltss",	ssecmp_32_insn,	4,	NONE,	0x05,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 1039 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpge_oqps",	ssecmp_128_insn,	3,	NONE,	0x1D,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 91 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmp",	arith_insn,	22,	NONE,	0x38,	0x07,	0,	0,	0,	0,	0},
#line 1195 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vhaddps",	xmm_xmm128_256_insn,	3,	NONE,	0xF2,	0x7C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 754 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacssww",	sse5pmacs_insn,	1,	NONE,	0x85,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1303 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermiltd2ps",	vpermil2_fixed_insn,	4,	NONE,	0x48,	0x00,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 334 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldcw",	fldnstcw_insn,	1,	NONE,	0x05,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1100 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngtsd",	ssecmp_64_insn,	4,	NONE,	0x0A,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 978 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"subss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5C,	0,	0,	CPU_SSE,	0,	0},
#line 824 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshad",	sse5psh_insn,	2,	NONE,	0x06,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 540 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsq",	onebyte_insn,	1,	NONE,	0xA5,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 1183 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubps",	fma_128_256_insn,	4,	NONE,	0x6C,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1223 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovdqu",	movau_insn,	6,	NONE,	0xF3,	0x6F,	0x10,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 353 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fninit",	twobyte_insn,	1,	NONE,	0xDB,	0xE3,	0,	0,	CPU_FPU,	0,	0},
#line 459 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jo",	jcc_insn,	9,	NONE,	0x00,	0,	0,	0,	0,	0,	0},
#line 1367 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsraw",	vpshift_insn,	4,	NONE,	0xE1,	0x71,	0x04,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 626 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 522 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movlpd",	movhlp_insn,	3,	NONE,	0x66,	0x12,	0,	0,	CPU_SSE2,	0,	0},
#line 261 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtsd2ss",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5A,	0,	0,	CPU_SSE2,	0,	0},
#line 655 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltd",	sse5comcc_insn,	1,	NONE,	0x4E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 143 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 315 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"femms",	twobyte_insn,	1,	NONE,	0x0F,	0x0E,	0,	0,	CPU_3DNow,	0,	0},
#line 45 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bswap",	bswap_insn,	2,	NONE,	0,	0,	0,	0,	CPU_486,	0,	0},
#line 94 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpeqsd",	ssecmp_64_insn,	4,	NONE,	0x00,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 1278 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpavgw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 905 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsts",	cyrixsmm_insn,	1,	NONE,	0x7D,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 1005 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vandpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x54,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 521 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movlhps",	movhllhps_insn,	2,	NONE,	0x16,	0,	0,	0,	CPU_SSE,	0,	0},
#line 643 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 273 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cwde",	onebyte_insn,	1,	NONE,	0x98,	0x20,	0,	0,	CPU_386,	0,	0},
#line 87 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovpe",	cmovcc_insn,	3,	NONE,	0x0A,	0,	0,	0,	CPU_686,	0,	0},
#line 699 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pf2id",	now3d_insn,	1,	NONE,	0x1D,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 407 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"haddps",	xmm_xmm128_insn,	2,	NONE,	0xF2,	0x7C,	0,	0,	CPU_SSE3,	0,	0},
#line 631 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomged",	sse5comcc_insn,	1,	NONE,	0x4E,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 934 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnle",	setcc_insn,	1,	NONE,	0x0F,	0,	0,	0,	CPU_386,	0,	0},
#line 124 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpsq",	onebyte_insn,	1,	NONE,	0xA7,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 1028 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeqsd",	ssecmp_64_insn,	4,	NONE,	0x00,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 691 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomw",	sse5com_insn,	1,	NONE,	0x4D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 651 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 835 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshufw",	pshufw_insn,	1,	NONE,	0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 311 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fdiv",	farith_insn,	7,	NONE,	0xF8,	0xF0,	0x06,	0,	CPU_FPU,	0,	0},
#line 762 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxsb",	sse4_insn,	2,	NONE,	0x3C,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 210 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuleps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 894 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rol",	shift_insn,	16,	NONE,	0x00,	0,	0,	0,	0,	0,	0},
#line 612 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomb",	sse5com_insn,	1,	NONE,	0x4C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 431 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"iretw",	onebyte_insn,	1,	NONE,	0xCF,	0x10,	0,	0,	0,	0,	0},
#line 712 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfrcpit1",	now3d_insn,	1,	NONE,	0xA6,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 388 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsub",	farith_insn,	7,	NONE,	0xE8,	0xE0,	0x04,	0,	CPU_FPU,	0,	0},
#line 1228 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovlpd",	movhlp_insn,	3,	NONE,	0x66,	0x12,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 928 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnbe",	setcc_insn,	1,	NONE,	0x07,	0,	0,	0,	CPU_386,	0,	0},
#line 75 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnc",	cmovcc_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_686,	0,	0},
#line 1315 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpinsrb",	pinsrb_insn,	4,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 57 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"clgi",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xDD,	0,	CPU_SVM,	0,	0},
#line 901 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsldt",	cyrixsmm_insn,	1,	NONE,	0x7B,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 418 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"insertps",	insertps_insn,	4,	NONE,	0,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 544 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsxd",	movsxd_insn,	1,	NONE,	0,	0,	0,	ONLY_64,	0,	0,	0},
#line 381 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsincos",	twobyte_insn,	1,	NONE,	0xD9,	0xFB,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 1384 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpunpckhwd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x69,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 33 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"andnpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x55,	0,	0,	CPU_SSE2,	0,	0},
#line 1245 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmptrst",	vmxtwobytemem_insn,	1,	NONE,	0x07,	0,	0,	0,	CPU_P4,	0,	0},
#line 147 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgtpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 946 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sgdt",	twobytemem_insn,	1,	NONE,	0x00,	0x0F,	0x01,	0,	CPU_286,	CPU_Priv,	0},
#line 719 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddbq",	sse5two_insn,	1,	NONE,	0x43,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 636 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 197 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comueqpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x08,	0,	0,	CPU_SSE5,	0,	0},
#line 481 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loadall",	twobyte_insn,	1,	NONE,	0x0F,	0x07,	0,	0,	CPU_386,	CPU_Undoc,	0},
#line 713 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfrcpit2",	now3d_insn,	1,	NONE,	0xB6,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 927 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnb",	setcc_insn,	1,	NONE,	0x03,	0,	0,	0,	CPU_386,	0,	0},
#line 514 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movddup",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x12,	0,	0,	CPU_SSE3,	0,	0},
#line 790 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhrwa",	now3d_insn,	1,	NONE,	0xB7,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 673 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 71 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovna",	cmovcc_insn,	3,	NONE,	0x06,	0,	0,	0,	CPU_686,	0,	0},
#line 874 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushfq",	onebyte_insn,	1,	NONE,	0x9C,	0x40,	0x40,	ONLY_64,	0,	0,	0},
#line 480 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lmsw",	prot286_insn,	1,	NONE,	0x06,	0x01,	0,	0,	CPU_286,	CPU_Priv,	0},
#line 940 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setp",	setcc_insn,	1,	NONE,	0x0A,	0,	0,	0,	CPU_386,	0,	0},
#line 640 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1416 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vunpcklps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x14,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1419 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vzeroall",	vzero_insn,	1,	NONE,	0xC4,	0,	0,	0,	CPU_AVX,	0,	0},
#line 639 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtd",	sse5comcc_insn,	1,	NONE,	0x4E,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1067 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpltps",	ssecmp_128_insn,	3,	NONE,	0x01,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 720 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddbw",	sse5two_insn,	1,	NONE,	0x41,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 420 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"insw",	onebyte_insn,	1,	NONE,	0x6D,	0x10,	0,	0,	0,	0,	0},
#line 867 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpcklwd",	mmxsse2_insn,	2,	NONE,	0x61,	0,	0,	0,	CPU_MMX,	0,	0},
#line 454 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnle",	jcc_insn,	9,	NONE,	0x0F,	0,	0,	0,	0,	0,	0},
#line 41 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"blendvps",	sse4xmm0_insn,	2,	NONE,	0x14,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 395 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fucomip",	fcom2_insn,	2,	NONE,	0xDF,	0xE8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 1117 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnltss",	ssecmp_32_insn,	4,	NONE,	0x05,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 896 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"roundpd",	sse4imm_insn,	2,	NONE,	0x09,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 728 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phadduwd",	sse5two_insn,	1,	NONE,	0x56,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 184 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnltss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1394 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vroundsd",	sse4m64imm_insn,	4,	NONE,	0x0B,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1229 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovlps",	movhlp_insn,	3,	NONE,	0x00,	0x12,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 925 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setna",	setcc_insn,	1,	NONE,	0x06,	0,	0,	0,	CPU_386,	0,	0},
#line 794 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulld",	sse4_insn,	2,	NONE,	0x40,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1414 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vunpckhps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x15,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1167 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdivss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5E,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 460 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jp",	jcc_insn,	9,	NONE,	0x0A,	0,	0,	0,	0,	0,	0},
#line 793 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhw",	mmxsse2_insn,	2,	NONE,	0xE5,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1317 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpinsrq",	pinsrq_insn,	2,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 386 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fstp",	fstp_insn,	4,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 107 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpneqss",	ssecmp_32_insn,	4,	NONE,	0x04,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 644 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 496 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lzcnt",	cnt_insn,	3,	NONE,	0xBD,	0,	0,	0,	CPU_686,	CPU_AMD,	0},
#line 1248 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmrun",	svm_rax_insn,	2,	NONE,	0xD8,	0,	0,	0,	CPU_SVM,	0,	0},
#line 1400 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsqrtpd",	avx_xmm_xmm128_insn,	2,	NONE,	0x66,	0x51,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1192 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmsubsd",	fma_128_m64_insn,	3,	NONE,	0x7F,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1004 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vandnps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x55,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1271 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddusb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDC,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 973 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"str",	str_insn,	4,	NONE,	0,	0,	0,	0,	CPU_286,	CPU_Prot,	0},
#line 271 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttss2si",	cvt_rx_xmm32_insn,	4,	NONE,	0xF3,	0x2C,	0,	0,	CPU_386,	CPU_SSE,	0},
#line 1234 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovntpd",	movnt_insn,	1,	NONE,	0x66,	0x2B,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1226 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovhps",	movhlp_insn,	3,	NONE,	0x00,	0x16,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 142 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comfalsess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0B,	0,	0,	CPU_SSE5,	0,	0},
#line 567 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"outsw",	onebyte_insn,	1,	NONE,	0x6F,	0x10,	0,	0,	0,	0,	0},
#line 861 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpckhdq",	mmxsse2_insn,	2,	NONE,	0x6A,	0,	0,	0,	CPU_MMX,	0,	0},
#line 971 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stosq",	onebyte_insn,	1,	NONE,	0xAB,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 968 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stmxcsr",	ldstmxcsr_insn,	1,	NONE,	0x03,	0,	0,	0,	CPU_SSE,	0,	0},
#line 280 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"divsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x5E,	0,	0,	CPU_SSE2,	0,	0},
#line 627 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 64 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovbe",	cmovcc_insn,	3,	NONE,	0x06,	0,	0,	0,	CPU_686,	0,	0},
#line 180 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnless",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 862 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpckhqdq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6D,	0,	0,	CPU_SSE2,	0,	0},
#line 1433 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xgetbv",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xD0,	0,	CPU_386,	CPU_XSAVE,	0},
#line 322 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fidivr",	fiarith_insn,	2,	NONE,	0x07,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 773 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pminuw",	sse4_insn,	2,	NONE,	0x3A,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1443 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xstore",	padlock_insn,	1,	NONE,	0xC0,	0x00,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 447 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnb",	jcc_insn,	9,	NONE,	0x03,	0,	0,	0,	0,	0,	0},
#line 1193 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmsubss",	fma_128_m32_insn,	3,	NONE,	0x7E,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 806 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popf",	onebyte_insn,	1,	NONE,	0x9D,	0x00,	0x40,	0,	0,	0,	0},
#line 597 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pclmullqlqdq",	pclmulqdq_fixed_insn,	1,	NONE,	0x00,	0,	0,	0,	CPU_CLMUL,	0,	0},
#line 937 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setns",	setcc_insn,	1,	NONE,	0x09,	0,	0,	0,	CPU_386,	0,	0},
#line 424 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"into",	onebyte_insn,	1,	NONE,	0xCE,	0,	0,	NOT_64,	0,	0,	0},
#line 1215 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmlaunch",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC2,	0,	CPU_P4,	0,	0},
#line 495 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ltr",	prot286_insn,	1,	NONE,	0x03,	0x00,	0,	0,	CPU_286,	CPU_Priv,	CPU_Prot},
#line 692 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pdistib",	cyrixmmx_insn,	1,	NONE,	0x54,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 414 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"in",	in_insn,	12,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 327 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fist",	fiarith_insn,	2,	NONE,	0x02,	0xDB,	0,	0,	CPU_FPU,	0,	0},
#line 1189 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmaddss",	fma_128_m32_insn,	3,	NONE,	0x7A,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 1355 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpshufb",	ssse3_insn,	3,	NONE,	0x00,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 576 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddd",	mmxsse2_insn,	2,	NONE,	0xFE,	0,	0,	0,	CPU_MMX,	0,	0},
#line 316 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ffree",	ffree_insn,	1,	NONE,	0xDD,	0,	0,	0,	CPU_FPU,	0,	0},
#line 427 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"invlpga",	invlpga_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SVM,	0,	0},
#line 851 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubq",	mmxsse2_insn,	2,	NONE,	0xFB,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1346 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmuldq",	ssse3_insn,	3,	NONE,	0x28,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 236 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunless",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 843 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psrad",	pshift_insn,	4,	NONE,	0xE2,	0x72,	0x04,	0,	CPU_MMX,	0,	0},
#line 1139 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunord_sps",	ssecmp_128_insn,	3,	NONE,	0x13,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1396 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vrsqrtps",	avx_xmm_xmm128_insn,	2,	NONE,	0x00,	0x52,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 235 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunlesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 285 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"enter",	enter_insn,	3,	NONE,	0,	0,	0,	0,	CPU_186,	0,	0},
#line 863 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpckhwd",	mmxsse2_insn,	2,	NONE,	0x69,	0,	0,	0,	CPU_MMX,	0,	0},
#line 382 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsqrt",	twobyte_insn,	1,	NONE,	0xD9,	0xFA,	0,	0,	CPU_FPU,	0,	0},
#line 403 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fxtract",	twobyte_insn,	1,	NONE,	0xD9,	0xF4,	0,	0,	CPU_FPU,	0,	0},
#line 1262 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpackssdw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6B,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 705 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfcmpgt",	now3d_insn,	1,	NONE,	0xA0,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 348 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmsubsd",	sse5arith64_insn,	8,	NONE,	0x0B,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 602 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpeqq",	sse4_insn,	2,	NONE,	0x29,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 106 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpneqsd",	ssecmp_64_insn,	4,	NONE,	0x04,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 566 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"outsd",	onebyte_insn,	1,	NONE,	0x6F,	0x20,	0,	0,	CPU_386,	0,	0},
#line 663 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomned",	sse5comcc_insn,	1,	NONE,	0x4E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1308 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphaddd",	ssse3_insn,	3,	NONE,	0x02,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 534 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movq",	movq_insn,	9,	NONE,	0,	0,	0,	0,	CPU_MMX,	0,	0},
#line 354 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmaddpd",	sse5arith_insn,	4,	NONE,	0x11,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 808 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popfq",	onebyte_insn,	1,	NONE,	0x9D,	0x40,	0x40,	ONLY_64,	0,	0,	0},
#line 251 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtpd2pi",	cvt_mm_xmm_insn,	1,	NONE,	0x66,	0x2D,	0,	0,	CPU_SSE2,	0,	0},
#line 661 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltw",	sse5comcc_insn,	1,	NONE,	0x4D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 630 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 891 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ret",	retnf_insn,	6,	NONE,	0xC2,	0,	0,	0,	0,	0,	0},
#line 1048 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgt_oqsd",	ssecmp_64_insn,	4,	NONE,	0x1E,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 706 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfmax",	now3d_insn,	1,	NONE,	0xA4,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 380 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsin",	twobyte_insn,	1,	NONE,	0xD9,	0xFE,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 532 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntsd",	movntsd_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE4a,	0,	0},
#line 577 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddq",	mmxsse2_insn,	2,	NONE,	0xD4,	0,	0,	0,	CPU_MMX,	0,	0},
#line 945 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sfence",	threebyte_insn,	1,	NONE,	0x0F,	0xAE,	0xF8,	0,	CPU_P3,	0,	0},
#line 533 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movntss",	movntss_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE4a,	0,	0},
#line 173 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngtpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 536 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movsb",	onebyte_insn,	1,	NONE,	0xA4,	0x00,	0,	0,	0,	0,	0},
#line 425 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"invd",	twobyte_insn,	1,	NONE,	0x0F,	0x08,	0,	0,	CPU_486,	CPU_Priv,	0},
#line 1142 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpunordpd",	ssecmp_128_insn,	3,	NONE,	0x03,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 320 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ficomp",	fiarith_insn,	2,	NONE,	0x03,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 1439 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xsave",	twobytemem_insn,	1,	NONE,	0x04,	0x0F,	0xAE,	0,	CPU_386,	CPU_XSAVE,	0},
#line 234 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunleps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 1247 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmresume",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xC3,	0,	CPU_P4,	0,	0},
#line 163 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comneqpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 1107 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnleps",	ssecmp_128_insn,	3,	NONE,	0x06,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 188 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comordss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 549 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mul",	f6_insn,	4,	NONE,	0x04,	0,	0,	0,	0,	0,	0},
#line 675 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 69 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovl",	cmovcc_insn,	3,	NONE,	0x0C,	0,	0,	0,	CPU_686,	0,	0},
#line 897 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"roundps",	sse4imm_insn,	2,	NONE,	0x08,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 759 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaddubsw",	ssse3_insn,	3,	NONE,	0x04,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 859 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ptest",	sse4_insn,	2,	NONE,	0x17,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 391 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fsubrp",	farithp_insn,	3,	NONE,	0xE0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 590 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pavgusb",	now3d_insn,	1,	NONE,	0xBF,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 545 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movupd",	movau_insn,	6,	NONE,	0x66,	0x10,	0x01,	0,	CPU_SSE2,	0,	0},
#line 671 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomnequw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1016 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_ossd",	ssecmp_64_insn,	4,	NONE,	0x10,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 312 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fdivp",	farithp_insn,	3,	NONE,	0xF8,	0,	0,	0,	CPU_FPU,	0,	0},
#line 471 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lea",	lea_insn,	3,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1031 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalse_osps",	ssecmp_128_insn,	3,	NONE,	0x1B,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 535 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movq2dq",	movq2dq_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 941 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setpe",	setcc_insn,	1,	NONE,	0x0A,	0,	0,	0,	CPU_386,	0,	0},
#line 1354 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsadbw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xF6,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 751 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacssdqh",	sse5pmacs_insn,	1,	NONE,	0x8F,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 117 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpordps",	ssecmp_128_insn,	3,	NONE,	0x07,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1221 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovddup",	vmovddup_insn,	3,	NONE,	0xF2,	0x12,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 227 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 232 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungtss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 245 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cpuid",	twobyte_insn,	1,	NONE,	0x0F,	0xA2,	0,	0,	CPU_486,	0,	0},
#line 1184 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubsd",	fma_128_m64_insn,	3,	NONE,	0x6F,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 396 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fucomp",	fcom2_insn,	2,	NONE,	0xDD,	0xE8,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 155 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comlesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 468 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lddqu",	lddqu_insn,	2,	NONE,	0,	0,	0,	0,	CPU_SSE3,	0,	0},
#line 1083 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneqps",	ssecmp_128_insn,	3,	NONE,	0x04,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 873 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushfd",	onebyte_insn,	1,	NONE,	0x9C,	0x20,	0,	NOT_64,	CPU_386,	0,	0},
#line 906 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sahf",	onebyte_insn,	1,	NONE,	0x9E,	0,	0,	0,	0,	0,	0},
#line 102 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpltsd",	ssecmp_64_insn,	4,	NONE,	0x01,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 966 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stgi",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xDC,	0,	CPU_SVM,	0,	0},
#line 1091 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngeps",	ssecmp_128_insn,	3,	NONE,	0x09,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 332 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fld",	fld_insn,	4,	NONE,	0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 333 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fld1",	twobyte_insn,	1,	NONE,	0xD9,	0xE8,	0,	0,	CPU_FPU,	0,	0},
#line 413 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"imul",	imul_insn,	19,	NONE,	0,	0,	0,	0,	0,	0,	0},
#line 1212 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vminps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x5D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 274 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"daa",	onebyte_insn,	1,	NONE,	0x27,	0,	0,	NOT_64,	0,	0,	0},
#line 834 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshuflw",	xmm_xmm128_imm_insn,	1,	NONE,	0xF2,	0x70,	0,	0,	CPU_SSE2,	0,	0},
#line 731 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddwd",	sse5two_insn,	1,	NONE,	0x46,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 594 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pclmulhqhqdq",	pclmulqdq_fixed_insn,	1,	NONE,	0x11,	0,	0,	0,	CPU_CLMUL,	0,	0},
#line 1268 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xD4,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 451 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jng",	jcc_insn,	9,	NONE,	0x0E,	0,	0,	0,	0,	0,	0},
#line 620 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomequw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 211 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comulesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0A,	0,	0,	CPU_SSE5,	0,	0},
#line 362 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnop",	twobyte_insn,	1,	NONE,	0xD9,	0xD0,	0,	0,	CPU_FPU,	0,	0},
#line 1115 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnltps",	ssecmp_128_insn,	3,	NONE,	0x05,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 816 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetcht2",	twobytemem_insn,	1,	NONE,	0x03,	0x0F,	0x18,	0,	CPU_P3,	0,	0},
#line 1344 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovzxwd",	sse4m64_insn,	2,	NONE,	0x33,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 299 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovnb",	fcmovcc_insn,	1,	NONE,	0xDB,	0xC0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 658 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltud",	sse5comcc_insn,	1,	NONE,	0x6E,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1334 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxbd",	sse4m32_insn,	2,	NONE,	0x21,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 204 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 938 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnz",	setcc_insn,	1,	NONE,	0x05,	0,	0,	0,	CPU_386,	0,	0},
#line 278 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"divpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5E,	0,	0,	CPU_SSE2,	0,	0},
#line 866 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpcklqdq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x6C,	0,	0,	CPU_SSE2,	0,	0},
#line 121 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpps",	xmm_xmm128_imm_insn,	1,	NONE,	0x00,	0xC2,	0,	0,	CPU_SSE,	0,	0},
#line 1244 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmptrld",	vmxtwobytemem_insn,	1,	NONE,	0x06,	0,	0,	0,	CPU_P4,	0,	0},
#line 1191 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfnmsubps",	fma_128_256_insn,	4,	NONE,	0x7C,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 560 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"o64",	NULL,	X86_OPERSIZE>>8,	0x40,	0,	0,	0,	ONLY_64,	0,	0,	0},
#line 822 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psadbw",	mmxsse2_insn,	2,	NONE,	0xF6,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1373 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xFA,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 922 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setge",	setcc_insn,	1,	NONE,	0x0D,	0,	0,	0,	CPU_386,	0,	0},
#line 264 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtss2sd",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x5A,	0,	0,	CPU_SSE2,	0,	0},
#line 1231 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovmskps",	movmsk_insn,	4,	NONE,	0x00,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 240 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunltss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 1237 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovsd",	movsd_insn,	5,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1295 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermil2ps",	vpermil2_insn,	4,	NONE,	0x48,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 76 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovne",	cmovcc_insn,	3,	NONE,	0x05,	0,	0,	0,	CPU_686,	0,	0},
#line 1393 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vroundps",	avx_sse4imm_insn,	3,	NONE,	0x08,	0,	0,	ONLY_AVX,	CPU_SSE41,	0,	0},
#line 219 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuneqpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 42 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bound",	bound_insn,	2,	NONE,	0,	0,	0,	NOT_64,	CPU_186,	0,	0},
#line 517 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movdqu",	movau_insn,	6,	NONE,	0xF3,	0x6F,	0x10,	0,	CPU_SSE2,	0,	0},
#line 847 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psrlq",	pshift_insn,	4,	NONE,	0xD3,	0x73,	0x02,	0,	CPU_MMX,	0,	0},
#line 889 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"repnz",	NULL,	X86_LOCKREP>>8,	0xF2,	0,	0,	0,	0,	0,	0,	0},
#line 1131 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmptrue_usps",	ssecmp_128_insn,	3,	NONE,	0x1F,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 511 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movapd",	movau_insn,	6,	NONE,	0x66,	0x28,	0x01,	0,	CPU_SSE2,	0,	0},
#line 1033 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalse_osss",	ssecmp_32_insn,	4,	NONE,	0x1B,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 596 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pclmullqhqdq",	pclmulqdq_fixed_insn,	1,	NONE,	0x10,	0,	0,	0,	CPU_CLMUL,	0,	0},
#line 478 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lidt",	twobytemem_insn,	1,	NONE,	0x03,	0x0F,	0x01,	0,	CPU_286,	CPU_Priv,	0},
#line 714 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfrsqit1",	now3d_insn,	1,	NONE,	0xA7,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 186 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comordps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 329 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fisttp",	fildstp_insn,	3,	NONE,	0x01,	0x00,	0x01,	0,	CPU_SSE3,	0,	0},
#line 722 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phadddq",	sse5two_insn,	1,	NONE,	0x4B,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1121 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpord_sss",	ssecmp_32_insn,	4,	NONE,	0x17,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 376 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"frstor",	onebytemem_insn,	1,	NONE,	0x04,	0xDD,	0,	0,	CPU_FPU,	0,	0},
#line 826 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshaw",	sse5psh_insn,	2,	NONE,	0x05,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 361 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fnmsubss",	sse5arith32_insn,	8,	NONE,	0x1A,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1235 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovntps",	movnt_insn,	1,	NONE,	0x00,	0x2B,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1273 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xFD,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1001 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddsubpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0xD0,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 721 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddd",	ssse3_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 239 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunltsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 796 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmuludq",	mmxsse2_insn,	2,	NONE,	0xF4,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 1287 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpgtb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x64,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1084 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneqsd",	ssecmp_64_insn,	4,	NONE,	0x04,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 435 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jbe",	jcc_insn,	9,	NONE,	0x06,	0,	0,	0,	0,	0,	0},
#line 767 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmaxuw",	sse4_insn,	2,	NONE,	0x3E,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1032 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalse_ossd",	ssecmp_64_insn,	4,	NONE,	0x1B,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1015 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_osps",	ssecmp_128_insn,	3,	NONE,	0x10,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 662 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 519 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movhpd",	movhlp_insn,	3,	NONE,	0x66,	0x16,	0,	0,	CPU_SSE2,	0,	0},
#line 283 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"dpps",	sse4imm_insn,	2,	NONE,	0x40,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 920 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sete",	setcc_insn,	1,	NONE,	0x04,	0,	0,	0,	CPU_386,	0,	0},
#line 1329 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminsw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xEA,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1057 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmple_oqss",	ssecmp_32_insn,	4,	NONE,	0x12,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 580 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddsw",	mmxsse2_insn,	2,	NONE,	0xED,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1279 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpblendvb",	avx_sse4xmm0_insn,	2,	NONE,	0x4C,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1328 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpminsd",	ssse3_insn,	3,	NONE,	0x39,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 301 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovne",	fcmovcc_insn,	1,	NONE,	0xDB,	0xC8,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 633 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 1149 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtdq2ps",	avx_xmm_xmm128_insn,	2,	NONE,	0x00,	0x5B,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1311 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphminposuw",	avx_ssse3_2op_insn,	1,	NONE,	0x41,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1430 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xcryptctr",	padlock_insn,	1,	NONE,	0xD8,	0xF3,	0xA7,	0,	CPU_PadLock,	0,	0},
#line 77 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovng",	cmovcc_insn,	3,	NONE,	0x0E,	0,	0,	0,	CPU_686,	0,	0},
#line 878 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rcpps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x53,	0,	0,	CPU_SSE,	0,	0},
#line 430 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"iretq",	onebyte_insn,	1,	NONE,	0xCF,	0x40,	0,	ONLY_64,	0,	0,	0},
#line 202 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugeps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 190 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comps",	sse5com_insn,	1,	NONE,	0x2C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 442 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jl",	jcc_insn,	9,	NONE,	0x0C,	0,	0,	0,	0,	0,	0},
#line 292 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fbld",	fbldstp_insn,	1,	NONE,	0x04,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1357 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpshufhw",	xmm_xmm128_imm_insn,	1,	NONE,	0xF3,	0x70,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1128 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpsd",	cmpsd_insn,	5,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 900 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rsdc",	rsdc_insn,	1,	NONE,	0,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 1350 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmulld",	ssse3_insn,	3,	NONE,	0x40,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 95 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpeqss",	ssecmp_32_insn,	4,	NONE,	0x00,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 385 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fstenv",	twobytemem_insn,	1,	NONE,	0x06,	0x9B,	0xD9,	0,	CPU_FPU,	0,	0},
#line 649 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 291 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"faddp",	farithp_insn,	3,	NONE,	0xC0,	0,	0,	0,	CPU_FPU,	0,	0},
#line 1369 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsrldq",	pslrldq_insn,	2,	NONE,	0x03,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 952 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shufps",	xmm_xmm128_imm_insn,	1,	NONE,	0x00,	0xC6,	0,	0,	CPU_SSE,	0,	0},
#line 562 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"orpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x56,	0,	0,	CPU_SSE2,	0,	0},
#line 1179 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddsubps",	fma_128_256_insn,	4,	NONE,	0x5C,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 259 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtps2pi",	cvt_mm_xmm64_insn,	2,	NONE,	0x2D,	0,	0,	0,	CPU_SSE,	0,	0},
#line 730 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddw",	ssse3_insn,	3,	NONE,	0x01,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 393 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fucom",	fcom2_insn,	2,	NONE,	0xDD,	0xE0,	0,	0,	CPU_286,	CPU_FPU,	0},
#line 1079 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_usps",	ssecmp_128_insn,	3,	NONE,	0x14,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 632 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 1314 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vphsubw",	ssse3_insn,	3,	NONE,	0x05,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 490 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loopne",	loop_insn,	8,	NONE,	0x00,	0,	0,	0,	0,	0,	0},
#line 433 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jae",	jcc_insn,	9,	NONE,	0x03,	0,	0,	0,	0,	0,	0},
#line 1441 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xsha1",	padlock_insn,	1,	NONE,	0xC8,	0xF3,	0xA6,	0,	CPU_PadLock,	0,	0},
#line 1069 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpltss",	ssecmp_32_insn,	4,	NONE,	0x01,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1040 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpge_oqsd",	ssecmp_64_insn,	4,	NONE,	0x1D,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 256 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtps2dq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5B,	0,	0,	CPU_SSE2,	0,	0},
#line 99 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpless",	ssecmp_32_insn,	4,	NONE,	0x02,	0xF3,	0,	0,	CPU_SSE,	0,	0},
#line 233 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunlepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x0E,	0,	0,	CPU_SSE5,	0,	0},
#line 1241 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovupd",	movau_insn,	6,	NONE,	0x66,	0x10,	0x01,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 479 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lldt",	prot286_insn,	1,	NONE,	0x02,	0x00,	0,	0,	CPU_286,	CPU_Priv,	CPU_Prot},
#line 321 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fidiv",	fiarith_insn,	2,	NONE,	0x06,	0xDA,	0,	0,	CPU_FPU,	0,	0},
#line 1364 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsllq",	vpshift_insn,	4,	NONE,	0xF3,	0x73,	0x06,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1368 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsrld",	vpshift_insn,	4,	NONE,	0xD2,	0x72,	0x02,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 349 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fmsubss",	sse5arith32_insn,	8,	NONE,	0x0A,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 856 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubusw",	mmxsse2_insn,	2,	NONE,	0xD9,	0,	0,	0,	CPU_MMX,	0,	0},
#line 558 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"o16",	NULL,	X86_OPERSIZE>>8,	0x10,	0,	0,	0,	0,	0,	0,	0},
#line 1412 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vucomiss",	avx_xmm_xmm32_insn,	2,	NONE,	0x00,	0x2E,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 243 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunordsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 105 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpneqps",	ssecmp_128_insn,	3,	NONE,	0x04,	0,	0,	0,	CPU_SSE,	0,	0},
#line 828 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshld",	sse5psh_insn,	2,	NONE,	0x02,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 282 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"dppd",	sse4imm_insn,	2,	NONE,	0x41,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 497 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"maskmovdqu",	maskmovdqu_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 1267 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpaddd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xFE,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 550 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"mulpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x59,	0,	0,	CPU_SSE2,	0,	0},
#line 145 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comgesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 434 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jb",	jcc_insn,	9,	NONE,	0x02,	0,	0,	0,	0,	0,	0},
#line 1182 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmsubpd",	fma_128_256_insn,	4,	NONE,	0x6D,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 986 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sysret",	twobyte_insn,	1,	NONE,	0x0F,	0x07,	0,	0,	CPU_686,	CPU_AMD,	CPU_Priv},
#line 1110 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlt_uqpd",	ssecmp_128_insn,	3,	NONE,	0x15,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1199 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vinsertps",	insertps_insn,	4,	NONE,	0xC0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1256 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmxon",	vmxthreebytemem_insn,	1,	NONE,	0xF3,	0,	0,	0,	CPU_P4,	0,	0},
#line 317 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ffreep",	ffree_insn,	1,	NONE,	0xDF,	0,	0,	0,	CPU_686,	CPU_FPU,	CPU_Undoc},
#line 956 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"smi",	onebyte_insn,	1,	NONE,	0xF1,	0,	0,	0,	CPU_386,	CPU_Undoc,	0},
#line 589 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pavgb",	mmxsse2_insn,	2,	NONE,	0xE0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1379 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xF9,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 810 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"por",	mmxsse2_insn,	2,	NONE,	0xEB,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1146 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcomisd",	avx_xmm_xmm64_insn,	2,	NONE,	0x66,	0x2F,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1277 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpavgb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE0,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1238 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovshdup",	avx_xmm_xmm128_insn,	2,	NONE,	0xF3,	0x16,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 214 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comultps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 657 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomltub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 1291 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpistri",	sse4pcmpstr_insn,	1,	NONE,	0x63,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 205 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugtpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1064 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplt_oqsd",	ssecmp_64_insn,	4,	NONE,	0x11,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 665 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneqb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1260 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpabsd",	avx_ssse3_2op_insn,	1,	NONE,	0x1E,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1090 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngepd",	ssecmp_128_insn,	3,	NONE,	0x09,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1081 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_usss",	ssecmp_32_insn,	4,	NONE,	0x14,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 513 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movd",	movd_insn,	8,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_MMX,	0},
#line 449 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnc",	jcc_insn,	9,	NONE,	0x03,	0,	0,	0,	0,	0,	0},
#line 437 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jcxz",	jcxz_insn,	2,	NONE,	0x10,	0,	0,	0,	0,	0,	0},
#line 992 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"umov",	umov_insn,	6,	NONE,	0,	0,	0,	0,	CPU_386,	CPU_Undoc,	0},
#line 1413 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vunpckhpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x15,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 306 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcomip",	fcom2_insn,	2,	NONE,	0xDF,	0xF0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 469 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ldmxcsr",	ldstmxcsr_insn,	1,	NONE,	0x02,	0,	0,	0,	CPU_SSE,	0,	0},
#line 54 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"clc",	onebyte_insn,	1,	NONE,	0xF8,	0,	0,	0,	0,	0,	0},
#line 1052 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgtsd",	ssecmp_64_insn,	4,	NONE,	0x0E,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 746 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmachriw",	pmachriw_insn,	1,	NONE,	0,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 297 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcmovbe",	fcmovcc_insn,	1,	NONE,	0xDA,	0xD0,	0,	0,	CPU_686,	CPU_FPU,	0},
#line 452 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnge",	jcc_insn,	9,	NONE,	0x0C,	0,	0,	0,	0,	0,	0},
#line 1000 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x58,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1002 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vaddsubps",	xmm_xmm128_256_insn,	3,	NONE,	0xF2,	0xD0,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 456 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jnp",	jcc_insn,	9,	NONE,	0x0B,	0,	0,	0,	0,	0,	0},
#line 189 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"compd",	sse5com_insn,	1,	NONE,	0x2D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 755 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacswd",	sse5pmacs_insn,	1,	NONE,	0x96,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1335 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmovsxbq",	sse4m16_insn,	2,	NONE,	0x22,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 871 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushaw",	onebyte_insn,	1,	NONE,	0x60,	0x10,	0,	NOT_64,	CPU_186,	0,	0},
#line 109 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnleps",	ssecmp_128_insn,	3,	NONE,	0x06,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1148 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcvtdq2pd",	avx_cvt_xmm64_insn,	3,	NONE,	0xF3,	0xE6,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 114 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnltsd",	ssecmp_64_insn,	4,	NONE,	0x05,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 473 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"les",	ldes_insn,	2,	NONE,	0xC4,	0,	0,	NOT_64,	0,	0,	0},
#line 1236 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovq",	vmovq_insn,	5,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1088 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnge_uqsd",	ssecmp_64_insn,	4,	NONE,	0x19,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 677 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomnew",	sse5comcc_insn,	1,	NONE,	0x4D,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 11 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"a16",	NULL,	X86_ADDRSIZE>>8,	0x10,	0,	0,	0,	0,	0,	0,	0},
#line 915 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"seta",	setcc_insn,	1,	NONE,	0x07,	0,	0,	0,	CPU_386,	0,	0},
#line 742 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pinsrb",	pinsrb_insn,	4,	NONE,	0,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 625 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalseub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 1204 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmaskmovps",	vmaskmov_insn,	4,	NONE,	0x2C,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 172 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngess",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 493 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lsl",	bsfr_insn,	3,	NONE,	0x03,	0,	0,	0,	CPU_286,	CPU_Prot,	0},
#line 462 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jpo",	jcc_insn,	9,	NONE,	0x0B,	0,	0,	0,	0,	0,	0},
#line 255 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtpi2ps",	cvt_xmm_mm_ps_insn,	1,	NONE,	0x2A,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1018 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_uqpd",	ssecmp_128_insn,	3,	NONE,	0x08,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 876 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pxor",	mmxsse2_insn,	2,	NONE,	0xEF,	0,	0,	0,	CPU_MMX,	0,	0},
#line 101 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpltps",	ssecmp_128_insn,	3,	NONE,	0x01,	0,	0,	0,	CPU_SSE,	0,	0},
#line 1276 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpandn",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xDF,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 883 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rdshr",	rdwrshr_insn,	1,	NONE,	0x00,	0,	0,	0,	CPU_686,	CPU_Cyrix,	CPU_SMM},
#line 823 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshab",	sse5psh_insn,	2,	NONE,	0x04,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 926 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setnae",	setcc_insn,	1,	NONE,	0x02,	0,	0,	0,	CPU_386,	0,	0},
#line 1037 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalsess",	ssecmp_32_insn,	4,	NONE,	0x0B,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 34 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"andnps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x55,	0,	0,	CPU_SSE,	0,	0},
#line 428 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"iret",	onebyte_insn,	1,	NONE,	0xCF,	0,	0,	0,	0,	0,	0},
#line 735 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phsubd",	ssse3_insn,	3,	NONE,	0x06,	0,	0,	0,	CPU_SSSE3,	0,	0},
#line 1165 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdivps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x5E,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 432 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"ja",	jcc_insn,	9,	NONE,	0x07,	0,	0,	0,	0,	0,	0},
#line 865 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"punpckldq",	mmxsse2_insn,	2,	NONE,	0x62,	0,	0,	0,	CPU_MMX,	0,	0},
#line 967 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sti",	onebyte_insn,	1,	NONE,	0xFB,	0,	0,	0,	0,	0,	0},
#line 854 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubsw",	mmxsse2_insn,	2,	NONE,	0xE9,	0,	0,	0,	CPU_MMX,	0,	0},
#line 641 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgtub",	sse5comcc_insn,	1,	NONE,	0x6C,	0x02,	0,	0,	CPU_SSE5,	0,	0},
#line 1073 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_oqss",	ssecmp_32_insn,	4,	NONE,	0x0C,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 162 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comneps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 509 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"montmul",	padlock_insn,	1,	NONE,	0xC0,	0xF3,	0xA6,	0,	CPU_PadLock,	0,	0},
#line 1284 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpeqw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x75,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 791 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmulhrwc",	cyrixmmx_insn,	1,	NONE,	0x59,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 79 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnl",	cmovcc_insn,	3,	NONE,	0x0D,	0,	0,	0,	CPU_686,	0,	0},
#line 803 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popad",	onebyte_insn,	1,	NONE,	0x61,	0x20,	0,	NOT_64,	CPU_386,	0,	0},
#line 996 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"unpcklps",	xmm_xmm128_insn,	2,	NONE,	0x00,	0x14,	0,	0,	CPU_SSE,	0,	0},
#line 1177 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vfmaddss",	fma_128_m32_insn,	3,	NONE,	0x6A,	0,	0,	ONLY_AVX,	CPU_FMA,	0,	0},
#line 595 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pclmulhqlqdq",	pclmulqdq_fixed_insn,	1,	NONE,	0x01,	0,	0,	0,	CPU_CLMUL,	0,	0},
#line 203 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comugesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 402 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fxsave",	twobytemem_insn,	1,	NONE,	0x00,	0x0F,	0xAE,	0,	CPU_686,	CPU_FPU,	0},
#line 681 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueq",	sse5comcc_insn,	1,	NONE,	0x4F,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 26 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"aesdec",	aes_insn,	1,	NONE,	0x38,	0xDE,	0,	0,	CPU_AES,	0,	0},
#line 1404 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vstmxcsr",	ldstmxcsr_insn,	1,	NONE,	0x03,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1036 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalsesd",	ssecmp_64_insn,	4,	NONE,	0x0B,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 32 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"and",	arith_insn,	22,	NONE,	0x20,	0x04,	0,	0,	0,	0,	0},
#line 177 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnlepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 607 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpgtd",	mmxsse2_insn,	2,	NONE,	0x66,	0,	0,	0,	CPU_MMX,	0,	0},
#line 870 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pushad",	onebyte_insn,	1,	NONE,	0x60,	0x20,	0,	NOT_64,	CPU_386,	0,	0},
#line 635 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomgeuq",	sse5comcc_insn,	1,	NONE,	0x6F,	0x03,	0,	0,	CPU_SSE5,	0,	0},
#line 756 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacsww",	sse5pmacs_insn,	1,	NONE,	0x95,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 127 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpunordpd",	ssecmp_128_insn,	3,	NONE,	0x03,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 1406 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vsubps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x5C,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1320 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmaddwd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xF5,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1290 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpgtw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x65,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 970 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"stosd",	onebyte_insn,	1,	NONE,	0xAB,	0x20,	0,	0,	CPU_386,	0,	0},
#line 1010 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vblendvps",	avx_sse4xmm0_insn,	2,	NONE,	0x4A,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1424 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"wrshr",	rdwrshr_insn,	1,	NONE,	0x01,	0,	0,	0,	CPU_686,	CPU_Cyrix,	CPU_SMM},
#line 74 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmovnbe",	cmovcc_insn,	3,	NONE,	0x07,	0,	0,	0,	CPU_686,	0,	0},
#line 171 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 1239 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovsldup",	avx_xmm_xmm128_insn,	2,	NONE,	0xF3,	0x12,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 708 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfmul",	now3d_insn,	1,	NONE,	0xB4,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 472 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"leave",	onebyte_insn,	1,	NONE,	0xC9,	0x00,	0x40,	0,	CPU_186,	0,	0},
#line 1019 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_uqps",	ssecmp_128_insn,	3,	NONE,	0x08,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1265 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpackuswb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x67,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 183 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnltsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 1281 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpeqb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x74,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 761 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmagw",	cyrixmmx_insn,	1,	NONE,	0x52,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 1375 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubsb",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE8,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 52 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cdq",	onebyte_insn,	1,	NONE,	0x99,	0x20,	0,	0,	CPU_386,	0,	0},
#line 909 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sar",	shift_insn,	16,	NONE,	0x07,	0,	0,	0,	0,	0,	0},
#line 489 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"loope",	loop_insn,	8,	NONE,	0x01,	0,	0,	0,	0,	0,	0},
#line 40 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"blendvpd",	sse4xmm0_insn,	2,	NONE,	0x15,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1092 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngesd",	ssecmp_64_insn,	4,	NONE,	0x09,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1251 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmulps",	xmm_xmm128_256_insn,	3,	NONE,	0x00,	0x59,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 22 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addsd",	xmm_xmm64_insn,	4,	NONE,	0xF2,	0x58,	0,	0,	CPU_SSE2,	0,	0},
#line 225 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comungepd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 1198 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vinsertf128",	vinsertf128_insn,	1,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1042 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgepd",	ssecmp_128_insn,	3,	NONE,	0x0D,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 110 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpnlesd",	ssecmp_64_insn,	4,	NONE,	0x06,	0xF2,	0,	0,	CPU_SSE2,	0,	0},
#line 266 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttpd2dq",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xE6,	0,	0,	CPU_SSE2,	0,	0},
#line 126 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpsw",	onebyte_insn,	1,	NONE,	0xA7,	0x10,	0,	0,	0,	0,	0},
#line 812 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetch",	twobytemem_insn,	1,	NONE,	0x00,	0x0F,	0x0D,	0,	CPU_3DNow,	0,	0},
#line 852 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubsb",	mmxsse2_insn,	2,	NONE,	0xE8,	0,	0,	0,	CPU_MMX,	0,	0},
#line 574 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"packuswb",	mmxsse2_insn,	2,	NONE,	0x67,	0,	0,	0,	CPU_MMX,	0,	0},
#line 1011 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vbroadcastf128",	vbroadcastf128_insn,	1,	NONE,	0,	0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 807 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"popfd",	onebyte_insn,	1,	NONE,	0x9D,	0x20,	0,	NOT_64,	CPU_386,	0,	0},
#line 980 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"svldt",	cyrixsmm_insn,	1,	NONE,	0x7A,	0,	0,	0,	CPU_486,	CPU_Cyrix,	CPU_SMM},
#line 749 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacsdql",	sse5pmacs_insn,	1,	NONE,	0x97,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 703 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfcmpeq",	now3d_insn,	1,	NONE,	0xB0,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 1168 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vdppd",	sse4imm_insn,	2,	NONE,	0x41,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1068 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpltsd",	ssecmp_64_insn,	4,	NONE,	0x01,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 716 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pfsub",	now3d_insn,	1,	NONE,	0x9A,	0,	0,	0,	CPU_3DNow,	0,	0},
#line 1196 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vhsubpd",	xmm_xmm128_256_insn,	3,	NONE,	0x66,	0x7D,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1017 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeq_osss",	ssecmp_32_insn,	4,	NONE,	0x10,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1246 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmread",	vmxmemrd_insn,	2,	NONE,	0,	0,	0,	0,	CPU_P4,	0,	0},
#line 1378 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpsubusw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xD9,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 265 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvtss2si",	cvt_rx_xmm32_insn,	4,	NONE,	0xF3,	0x2D,	0,	0,	CPU_386,	CPU_SSE,	0},
#line 46 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bt",	bittest_insn,	6,	NONE,	0xA3,	0x04,	0,	0,	CPU_386,	0,	0},
#line 853 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubsiw",	cyrixmmx_insn,	1,	NONE,	0x55,	0,	0,	0,	CPU_Cyrix,	CPU_MMX,	0},
#line 104 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cmpneqpd",	ssecmp_128_insn,	3,	NONE,	0x04,	0x66,	0,	0,	CPU_SSE,	0,	0},
#line 223 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunesd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 269 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"cvttps2pi",	cvt_mm_xmm64_insn,	2,	NONE,	0x2C,	0,	0,	0,	CPU_SSE,	0,	0},
#line 518 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movhlps",	movhllhps_insn,	2,	NONE,	0x12,	0,	0,	0,	CPU_SSE,	0,	0},
#line 573 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"packusdw",	sse4_insn,	2,	NONE,	0x2B,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 220 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuneqps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 985 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"sysexit",	twobyte_insn,	1,	NONE,	0x0F,	0x35,	0,	NOT_64,	CPU_686,	CPU_Priv,	0},
#line 1299 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpermilmz2ps",	vpermil2_fixed_insn,	4,	NONE,	0x48,	0x03,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 168 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comness",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 504 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"minpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x5D,	0,	0,	CPU_SSE2,	0,	0},
#line 582 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"paddusw",	mmxsse2_insn,	2,	NONE,	0xDD,	0,	0,	0,	CPU_MMX,	0,	0},
#line 218 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuneps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 1099 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpngtps",	ssecmp_128_insn,	3,	NONE,	0x0A,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 646 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomleb",	sse5comcc_insn,	1,	NONE,	0x4C,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 1058 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplepd",	ssecmp_128_insn,	3,	NONE,	0x02,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 304 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fcom",	fcom_insn,	5,	NONE,	0xD0,	0x02,	0,	0,	CPU_FPU,	0,	0},
#line 1035 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalseps",	ssecmp_128_insn,	3,	NONE,	0x0B,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1390 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vrcpps",	avx_xmm_xmm128_insn,	2,	NONE,	0x00,	0x53,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 849 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psubb",	mmxsse2_insn,	2,	NONE,	0xF8,	0,	0,	0,	CPU_MMX,	0,	0},
#line 18 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"adc",	arith_insn,	22,	NONE,	0x10,	0x02,	0,	0,	0,	0,	0},
#line 888 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"repne",	NULL,	X86_LOCKREP>>8,	0xF2,	0,	0,	0,	0,	0,	0,	0},
#line 35 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"andpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x54,	0,	0,	CPU_SSE2,	0,	0},
#line 666 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomneqd",	sse5comcc_insn,	1,	NONE,	0x4E,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 341 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fldz",	twobyte_insn,	1,	NONE,	0xD9,	0xEE,	0,	0,	CPU_FPU,	0,	0},
#line 881 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rdmsr",	twobyte_insn,	1,	NONE,	0x0F,	0x32,	0,	0,	CPU_586,	CPU_Priv,	0},
#line 465 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jz",	jcc_insn,	9,	NONE,	0x04,	0,	0,	0,	0,	0,	0},
#line 611 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcmpistrm",	sse4pcmpstr_insn,	1,	NONE,	0x62,	0,	0,	0,	CPU_SSE42,	0,	0},
#line 1049 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgt_oqss",	ssecmp_32_insn,	4,	NONE,	0x1E,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 398 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"fwait",	onebyte_insn,	1,	NONE,	0x9B,	0,	0,	0,	CPU_FPU,	0,	0},
#line 698 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pextrw",	pextrw_insn,	7,	NONE,	0,	0,	0,	0,	CPU_MMX,	CPU_P3,	0},
#line 1288 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpcmpgtd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 276 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"dec",	incdec_insn,	6,	NONE,	0x48,	0x01,	0,	0,	0,	0,	0},
#line 485 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lodsd",	onebyte_insn,	1,	NONE,	0xAD,	0x20,	0,	0,	CPU_386,	0,	0},
#line 467 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"lar",	bsfr_insn,	3,	NONE,	0x02,	0,	0,	0,	CPU_286,	CPU_Prot,	0},
#line 653 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomlew",	sse5comcc_insn,	1,	NONE,	0x4D,	0x01,	0,	0,	CPU_SSE5,	0,	0},
#line 165 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comneqsd",	sse5comcc64_insn,	2,	NONE,	0x2F,	0x04,	0,	0,	CPU_SSE5,	0,	0},
#line 727 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"phaddudq",	sse5two_insn,	1,	NONE,	0x5B,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 1351 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vpmullw",	xmm_xmm128_insn,	2,	NONE,	0x66,	0xD5,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 832 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshufd",	xmm_xmm128_imm_insn,	1,	NONE,	0x66,	0x70,	0,	0,	CPU_SSE2,	0,	0},
#line 43 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"bsf",	bsfr_insn,	3,	NONE,	0xBC,	0,	0,	0,	CPU_386,	0,	0},
#line 825 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pshaq",	sse5psh_insn,	2,	NONE,	0x07,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 846 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"psrldq",	pslrldq_insn,	2,	NONE,	0x03,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 623 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomfalsed",	sse5comcc_insn,	1,	NONE,	0x4E,	0x06,	0,	0,	CPU_SSE5,	0,	0},
#line 885 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"rdtscp",	threebyte_insn,	1,	NONE,	0x0F,	0x01,	0xF9,	0,	CPU_686,	CPU_AMD,	CPU_Priv},
#line 931 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setng",	setcc_insn,	1,	NONE,	0x0E,	0,	0,	0,	CPU_386,	0,	0},
#line 957 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"smint",	twobyte_insn,	1,	NONE,	0x0F,	0x38,	0,	0,	CPU_686,	CPU_Cyrix,	0},
#line 918 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"setbe",	setcc_insn,	1,	NONE,	0x06,	0,	0,	0,	CPU_386,	0,	0},
#line 409 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"hsubpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x7D,	0,	0,	CPU_SSE3,	0,	0},
#line 1034 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpfalsepd",	ssecmp_128_insn,	3,	NONE,	0x0B,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 776 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmovsxbq",	sse4m16_insn,	2,	NONE,	0x22,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 1436 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"xorpd",	xmm_xmm128_insn,	2,	NONE,	0x66,	0x57,	0,	0,	CPU_SSE2,	0,	0},
#line 1227 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vmovlhps",	movhllhps_insn,	2,	NONE,	0x16,	0xC0,	0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1044 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpgesd",	ssecmp_64_insn,	4,	NONE,	0x0D,	0xF2,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 224 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuness",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 222 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comuneqss",	sse5comcc32_insn,	2,	NONE,	0x2E,	0x0C,	0,	0,	CPU_SSE5,	0,	0},
#line 748 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pmacsdqh",	sse5pmacs_insn,	1,	NONE,	0x9F,	0,	0,	0,	CPU_SSE5,	0,	0},
#line 461 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"jpe",	jcc_insn,	9,	NONE,	0x0A,	0,	0,	0,	0,	0,	0},
#line 1119 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpord_sps",	ssecmp_128_insn,	3,	NONE,	0x17,	0x00,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1102 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnle_uqpd",	ssecmp_128_insn,	3,	NONE,	0x16,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 744 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pinsrq",	pinsrq_insn,	2,	NONE,	0,	0,	0,	ONLY_64,	CPU_SSE41,	0,	0},
#line 911 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"scasb",	onebyte_insn,	1,	NONE,	0xAE,	0x00,	0,	0,	0,	0,	0},
#line 170 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comngeps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x09,	0,	0,	CPU_SSE5,	0,	0},
#line 950 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"shrd",	shlrd_insn,	9,	NONE,	0xAC,	0,	0,	0,	CPU_386,	0,	0},
#line 1029 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpeqss",	ssecmp_32_insn,	4,	NONE,	0x00,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1065 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmplt_oqss",	ssecmp_32_insn,	4,	NONE,	0x11,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 515 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"movdq2q",	movdq2q_insn,	1,	NONE,	0,	0,	0,	0,	CPU_SSE2,	0,	0},
#line 135 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comeqpd",	sse5comcc_insn,	1,	NONE,	0x2D,	0x00,	0,	0,	CPU_SSE5,	0,	0},
#line 813 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"prefetchnta",	twobytemem_insn,	1,	NONE,	0x00,	0x0F,	0x18,	0,	CPU_P3,	0,	0},
#line 685 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pcomtrueuw",	sse5comcc_insn,	1,	NONE,	0x6D,	0x07,	0,	0,	CPU_SSE5,	0,	0},
#line 1074 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpneq_ospd",	ssecmp_128_insn,	3,	NONE,	0x1C,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1038 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpge_oqpd",	ssecmp_128_insn,	3,	NONE,	0x1D,	0x66,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 1113 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"vcmpnlt_uqss",	ssecmp_32_insn,	4,	NONE,	0x15,	0xF3,	0xC0,	ONLY_AVX,	CPU_AVX,	0,	0},
#line 238 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comunltps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x0D,	0,	0,	CPU_SSE5,	0,	0},
#line 592 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"pblendvb",	sse4xmm0_insn,	2,	NONE,	0x10,	0,	0,	0,	CPU_SSE41,	0,	0},
#line 182 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"comnltps",	sse5comcc_insn,	1,	NONE,	0x2C,	0x05,	0,	0,	CPU_SSE5,	0,	0},
#line 23 "/home/kleber.kruger/sniper-benchmarks/parsec/parsec-2.1/./pkgs/tools/yasm/src/x86insn_nasm.gperf"
    {"addss",	xmm_xmm32_insn,	4,	NONE,	0xF3,	0x58,	0,	0,	CPU_SSE,	0,	0}
  };
  static const unsigned short tab[] = {
    1213,764,567,0,0,0,1095,1432,605,11,281,1000,1140,323,1044,0,
    0,427,0,281,702,11,83,83,0,323,702,427,323,0,281,764,
    1091,64,1140,702,764,1044,0,1042,0,630,988,1140,1140,1312,0,630,
    323,83,665,133,0,702,323,11,764,83,1312,197,665,1481,746,1140,
    0,11,702,508,0,323,764,864,864,0,665,281,0,517,0,1165,
    1140,1140,1312,1140,1481,702,1312,11,1432,764,988,721,0,665,702,0,
    1140,323,0,83,323,83,764,1481,0,665,1140,281,823,1189,1116,323,
    891,1312,1140,0,1074,0,665,358,288,83,0,605,829,1140,988,0,
    0,1042,1140,1312,574,11,764,0,1279,1042,1116,0,517,11,0,83,
    281,323,0,1109,197,83,0,665,1089,1140,605,323,764,1042,988,1213,
    1218,1140,1140,764,1481,517,630,1042,1034,0,764,0,1109,323,399,988,
    665,1109,1140,1218,159,348,746,1042,1042,781,323,764,1399,0,517,764,
    11,11,1140,281,83,1185,1027,0,1044,0,604,517,0,665,1042,517,
    0,1140,702,1044,909,1509,1042,323,333,1312,1042,764,11,528,1034,665,
    1140,1213,764,399,0,1481,323,1338,764,630,517,1109,111,1140,823,348,
    567,702,630,508,665,0,288,597,83,1109,427,64,864,440,702,1185,
    0,1140,630,281,1312,1212,0,764,864,1213,1399,281,1074,348,1312,665,
    0,1109,665,629,506,823,64,517,180,764,823,427,815,891,1116,281,
    764,605,733,988,829,0,630,702,427,94,1338,517,864,1140,0,323,
    517,508,1089,803,323,826,83,553,525,1109,1103,11,0,764,702,1044,
    1034,11,823,1105,323,338,517,281,1432,764,665,1213,197,764,11,517,
    894,0,1089,764,630,605,728,1044,656,702,864,665,665,1379,574,1399,
    0,702,159,924,403,793,762,1140,1203,0,702,1116,0,793,630,630,
    702,0,1204,793,1252,764,94,1295,179,1213,517,1044,333,1189,1050,605,
    281,793,1236,440,517,1140,271,1379,281,187,864,824,76,358,1338,0,
    1312,864,51,11,0,323,444,746,1095,323,1044,764,10,824,83,47,
    1481,0,0,764,0,702,764,0,1140,1312,1116,399,0,764,1212,574,
    403,109,764,1477,665,1195,629,857,399,1042,0,764,693,0,702,51,
    747,567,1312,843,937,0,1082,333,1119,1185,864,11,1263,1185,11,755,
    336,629,1159,629,799,83,764,597,1411,1312,900,0,799,829,0,1312,
    338,552,1162,0,933,1140,1223,1287,11,1043,232,1530,702,665,1105,1116,
    281,1140,0,793,197,0,1232,0,351,0,1140,1101,764,704,605,188,
  };

  const struct insnprefix_parse_data *ret;
  unsigned long rsl, val = phash_lookup(key, len, 0xbe1e08bbUL);
  rsl = ((val>>23)^tab[val&0x1ff]);
  if (rsl >= 1434) return NULL;
  ret = &pd[rsl];
  if (strcmp(key, ret->name) != 0) return NULL;
  return ret;
}

