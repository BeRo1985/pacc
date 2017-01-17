unit PACCLinker_ELF_ELF;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap,PACCLinker;

const ET_NONE=0;
      ET_REL=1;
      ET_EXEC=2;
      ET_DYN=3; 
      ET_CORE=4; 
      ET_LOOS=$fe00; 
      ET_HIOS=$feff; 
      ET_LOPROC=$ff00;
      ET_HIPROC=$ffff;

      EM_NONE=0; // No machine
      EM_M32=1; // AT&T WE 32100
      EM_SPARC=2; // SUN SPARC
      EM_386=3; // Intel 80386
      EM_68K=4; // Motorola m68k family
      EM_88K=5; // Motorola m88k family
      EM_486=6; // Reserved for future use
      EM_860=7; // Intel 80860
      EM_MIPS=8; // MIPS R3000 (officially, big-endian only)
      EM_S370=9; // IBM System/370
      EM_MIPS_RS3_LE=10; // MIPS R3000 little-endian (Oct 4 1999 Draft) Deprecated
      EM_res011=11; // Reserved
      EM_res012=12; // Reserved
      EM_res013=13; // Reserved
      EM_res014=14; // Reserved
      EM_PARISC=15; // HPPA
      EM_res016=16; // Reserved
      EM_VPP550=17; // Fujitsu VPP500
      EM_SPARC32PLUS=18; // Sun's "v8plus"
      EM_960=19; // Intel 80960
      EM_PPC=20; // PowerPC
      EM_PPC64=21; // 64-bit PowerPC
      EM_S390=22; // IBM S/390
      EM_SPU=23; // Sony/Toshiba/IBM SPU
      EM_res024=24; // Reserved
      EM_res025=25; // Reserved
      EM_res026=26; // Reserved
      EM_res027=27; // Reserved
      EM_res028=28; // Reserved
      EM_res029=29; // Reserved
      EM_res030=30; // Reserved
      EM_res031=31; // Reserved
      EM_res032=32; // Reserved
      EM_res033=33; // Reserved
      EM_res034=34; // Reserved
      EM_res035=35; // Reserved
      EM_V800=36; // NEC V800 series
      EM_FR20=37; // Fujitsu FR20
      EM_RH32=38; // TRW RH32
      EM_MCORE=39; // May also be taken by Fujitsu MMA
      EM_RCE=39; // Old name for MCore
      EM_ARM=40; // ARM
      EM_OLD_ALPHA=41; // Digital Alpha
      EM_SH=42; // Renesas (formerly Hitachi) / SuperH SH
      EM_SPARCV9=43; // SPARC v9 64-bit
      EM_TRICORE=44; // Siemens Tricore embedded processor
      EM_ARC=45; // ARC Cores
      EM_H8_300=46; // Renesas (formerly Hitachi) H8/300
      EM_H8_300H=47; // Renesas (formerly Hitachi) H8/300H
      EM_H8S=48; // Renesas (formerly Hitachi) H8S
      EM_H8_500=49; // Renesas (formerly Hitachi) H8/500
      EM_IA_64=50; // Intel IA-64 Processor
      EM_MIPS_X=51; // Stanford MIPS-X
      EM_COLDFIRE=52; // Motorola Coldfire
      EM_68HC12=53; // Motorola M68HC12
      EM_MMA=54; // Fujitsu Multimedia Accelerator
      EM_PCP=55; // Siemens PCP
      EM_NCPU=56; // Sony nCPU embedded RISC processor
      EM_NDR1=57; // Denso NDR1 microprocesspr
      EM_STARCORE=58; // Motorola Star*Core processor
      EM_ME16=59; // Toyota ME16 processor
      EM_ST100=60; // STMicroelectronics ST100 processor
      EM_TINYJ=61; // Advanced Logic Corp. TinyJ embedded processor
      EM_X86_64=62; // Advanced Micro Devices X86-64 processor
      EM_PDSP=63; // Sony DSP Processor
      EM_PDP10=64; // Digital Equipment Corp. PDP-10
      EM_PDP11=65; // Digital Equipment Corp. PDP-11
      EM_FX66=66; // Siemens FX66 microcontroller
      EM_ST9PLUS=67; // STMicroelectronics ST9+ 8/16 bit microcontroller
      EM_ST7=68; // STMicroelectronics ST7 8-bit microcontroller
      EM_68HC16=69; // Motorola MC68HC16 Microcontroller
      EM_68HC11=70; // Motorola MC68HC11 Microcontroller
      EM_68HC08=71; // Motorola MC68HC08 Microcontroller
      EM_68HC05=72; // Motorola MC68HC05 Microcontroller
      EM_SVX=73; // Silicon Graphics SVx
      EM_ST19=74; // STMicroelectronics ST19 8-bit cpu
      EM_VAX=75; // Digital VAX
      EM_CRIS=76; // Axis Communications 32-bit embedded processor
      EM_JAVELIN=77; // Infineon Technologies 32-bit embedded cpu
      EM_FIREPATH=78; // Element 14 64-bit DSP processor
      EM_ZSP=79; // LSI Logic's 16-bit DSP processor
      EM_MMIX=80; // Donald Knuth's educational 64-bit processor
      EM_HUANY=81; // Harvard's machine-independent format
      EM_PRISM=82; // SiTera Prism
      EM_AVR=83; // Atmel AVR 8-bit microcontroller
      EM_FR30=84; // Fujitsu FR30
      EM_D10V=85; // Mitsubishi D10V
      EM_D30V=86; // Mitsubishi D30V
      EM_V850=87; // NEC v850
      EM_M32R=88; // Renesas M32R (formerly Mitsubishi M32R)
      EM_MN10300=89; // Matsushita MN10300
      EM_MN10200=90; // Matsushita MN10200
      EM_PJ=91; // picoJava
      EM_OPENRISC=92; // OpenRISC 32-bit embedded processor
      EM_ARC_A5=93; // ARC Cores Tangent-A5
      EM_XTENSA=94; // Tensilica Xtensa Architecture
      EM_VIDEOCORE=95; // Alphamosaic VideoCore processor
      EM_TMM_GPP=96; // Thompson Multimedia General Purpose Processor
      EM_NS32K=97; // National Semiconductor 32000 series
      EM_TPC=98; // Tenor Network TPC processor
      EM_SNP1K=99; // Trebia SNP 1000 processor
      EM_ST200=100; // STMicroelectronics ST200 microcontroller
      EM_IP2K=101; // Ubicom IP2022 micro controller
      EM_MAX=102; // MAX Processor
      EM_CR=103; // National Semiconductor CompactRISC
      EM_F2MC16=104; // Fujitsu F2MC16
      EM_MSP430=105; // TI msp430 micro controller
      EM_BLACKFIN=106; // ADI Blackfin
      EM_SE_C33=107; // S1C33 Family of Seiko Epson processors
      EM_SEP=108; // Sharp embedded microprocessor
      EM_ARCA=109; // Arca RISC Microprocessor
      EM_UNICORE=110; // Microprocessor series from PKU-Unity Ltd. and MPRC of Peking University
      EM_EXCESS=111; // eXcess: 16/32/64-bit configurable embedded CPU
      EM_DXP=112; // Icera Semiconductor Inc. Deep Execution Processor
      EM_ALTERA_NIOS2=113; // Altera Nios II soft-core processor
      EM_CRX=114; // National Semiconductor CRX
      EM_XGATE=115; // Motorola XGATE embedded processor
      EM_C166=116; // Infineon C16x/XC16x processor
      EM_M16C=117; // Renesas M16C series microprocessors
      EM_DSPIC30F=118; // Microchip Technology dsPIC30F Digital Signal Controller
      EM_CE=119; // Freescale Communication Engine RISC core
      EM_M32C=120; // Renesas M32C series microprocessors
      EM_res121=121; // Reserved
      EM_res122=122; // Reserved
      EM_res123=123; // Reserved
      EM_res124=124; // Reserved
      EM_res125=125; // Reserved
      EM_res126=126; // Reserved
      EM_res127=127; // Reserved
      EM_res128=128; // Reserved
      EM_res129=129; // Reserved
      EM_res130=130; // Reserved
      EM_TSK3000=131; // Altium TSK3000 core
      EM_RS08=132; // Freescale RS08 embedded processor
      EM_res133=133; // Reserved
      EM_ECOG2=134; // Cyan Technology eCOG2 microprocessor
      EM_SCORE=135; // Sunplus Score
      EM_SCORE7=135; // Sunplus S+core7 RISC processor
      EM_DSP24=136; // New Japan Radio (NJR) 24-bit DSP Processor
      EM_VIDEOCORE3=137; // Broadcom VideoCore III processor
      EM_LATTICEMICO32=138; // RISC processor for Lattice FPGA architecture
      EM_SE_C17=139; // Seiko Epson C17 family
      EM_TI_C6000=140; // Texas Instruments TMS320C6000 DSP family
      EM_TI_C2000=141; // Texas Instruments TMS320C2000 DSP family
      EM_TI_C5500=142; // Texas Instruments TMS320C55x DSP family
      EM_res143=143; // Reserved
      EM_res144=144; // Reserved
      EM_res145=145; // Reserved
      EM_res146=146; // Reserved
      EM_res147=147; // Reserved
      EM_res148=148; // Reserved
      EM_res149=149; // Reserved
      EM_res150=150; // Reserved
      EM_res151=151; // Reserved
      EM_res152=152; // Reserved
      EM_res153=153; // Reserved
      EM_res154=154; // Reserved
      EM_res155=155; // Reserved
      EM_res156=156; // Reserved
      EM_res157=157; // Reserved
      EM_res158=158; // Reserved
      EM_res159=159; // Reserved
      EM_MMDSP_PLUS=160; // STMicroelectronics 64bit VLIW Data Signal Processor
      EM_CYPRESS_M8C=161; // Cypress M8C microprocessor
      EM_R32C=162; // Renesas R32C series microprocessors
      EM_TRIMEDIA=163; // NXP Semiconductors TriMedia architecture family
      EM_QDSP6=164; // QUALCOMM DSP6 Processor
      EM_8051=165; // Intel 8051 and variants
      EM_STXP7X=166; // STMicroelectronics STxP7x family
      EM_NDS32=167; // Andes Technology compact code size embedded RISC processor family
      EM_ECOG1=168; // Cyan Technology eCOG1X family
      EM_ECOG1X=168; // Cyan Technology eCOG1X family
      EM_MAXQ30=169; // Dallas Semiconductor MAXQ30 Core Micro-controllers
      EM_XIMO16=170; // New Japan Radio (NJR) 16-bit DSP Processor
      EM_MANIK=171; // M2000 Reconfigurable RISC Microprocessor
      EM_CRAYNV2=172; // Cray Inc. NV2 vector architecture
      EM_RX=173; // Renesas RX family
      EM_METAG=174; // Imagination Technologies META processor architecture
      EM_MCST_ELBRUS=175; // MCST Elbrus general purpose hardware architecture
      EM_ECOG16=176; // Cyan Technology eCOG16 family
      EM_CR16=177; // National Semiconductor CompactRISC 16-bit processor
      EM_ETPU=178; // Freescale Extended Time Processing Unit
      EM_SLE9X=179; // Infineon Technologies SLE9X core
      EM_L1OM=180; // Intel L1OM
      EM_INTEL181=181; // Reserved by Intel
      EM_INTEL182=182; // Reserved by Intel
      EM_res183=183; // Reserved by ARM
      EM_res184=184; // Reserved by ARM
      EM_AVR32=185; // Atmel Corporation 32-bit microprocessor family
      EM_STM8=186; // STMicroeletronics STM8 8-bit microcontroller
      EM_TILE64=187; // Tilera TILE64 multicore architecture family
      EM_TILEPRO=188; // Tilera TILEPro multicore architecture family
      EM_MICROBLAZE=189; // Xilinx MicroBlaze 32-bit RISC soft processor core
      EM_CUDA=190; // NVIDIA CUDA architecture 
      EM_TILEGX=191; // Tilera TILE-Gx multicore architecture family
      EM_CLOUDSHIELD=192; // CloudShield architecture family
      EM_COREA_1ST=193; // KIPO-KAIST Core-A 1st generation processor family
      EM_COREA_2ND=194; // KIPO-KAIST Core-A 2nd generation processor family
      EM_ARC_COMPACT2=195; // Synopsys ARCompact V2
      EM_OPEN8=196; // Open8 8-bit RISC soft processor core
      EM_RL78=197; // Renesas RL78 family
      EM_VIDEOCORE5=198; // Broadcom VideoCore V processor
      EM_78KOR=199; // Renesas 78KOR family
      EM_56800EX=200; // Freescale 56800EX Digital Signal Controller (DSC)
      EM_BA1=201; // Beyond BA1 CPU architecture
      EM_BA2=202; // Beyond BA2 CPU architecture
      EM_XCORE=203; // XMOS xCORE processor family
      EM_MCHP_PIC=204; // Microchip 8-bit PIC(r) family
      EM_INTEL205=205; // Reserved by Intel
      EM_INTEL206=206; // Reserved by Intel
      EM_INTEL207=207; // Reserved by Intel
      EM_INTEL208=208; // Reserved by Intel
      EM_INTEL209=209; // Reserved by Intel
      EM_KM32=210; // KM211 KM32 32-bit processor
      EM_KMX32=211; // KM211 KMX32 32-bit processor
      EM_KMX16=212; // KM211 KMX16 16-bit processor
      EM_KMX8=213; // KM211 KMX8 8-bit processor
      EM_KVARC=214; // KM211 KVARC processor
      EM_CDP=215; // Paneve CDP architecture family
      EM_COGE=216; // Cognitive Smart Memory Processor
      EM_COOL=217; // iCelero CoolEngine
      EM_NORC=218; // Nanoradio Optimized RISC
      EM_CSR_KALIMBA=219; // CSR Kalimba architecture family
      EM_Z80=220; // Zilog Z80
      EM_VISIUM=221; // Controls and Data Services VISIUMcore processor
      EM_FT32=222; // FTDI Chip FT32 high performance 32-bit RISC architecture
      EM_MOXIE=223; // Moxie processor family
      EM_AMDGPU=224; // AMD GPU architecture
      EM_RISCV=243; // RISC-V
      EM_LANAI=244; // Lanai processor
      EM_CEVA=245; // CEVA Processor Architecture Family
      EM_CEVA_X2=246; // CEVA X2 Processor Family
      EM_BPF=247; // Linux BPF – in-kernel virtual machine

      // File version
      EV_NONE=0; 
      EV_CURRENT=1; 

      // Identification index
      EI_MAG0=0;
      EI_MAG1=1; 
      EI_MAG2=2; 
      EI_MAG3=3; 
      EI_CLASS=4; 
      EI_DATA=5;
      EI_VERSION=6; 
      EI_OSABI=7; 
      EI_ABIVERSION=8; 
      EI_PAD=9; 
      EI_NIDENT=16;

      // Magic number
      ELFMAG0=$7f;
      ELFMAG1=ord('E');
      ELFMAG2=ord('L');
      ELFMAG3=ord('F');

      // File class
      ELFCLASSNONE=0; 
      ELFCLASS32=1; 
      ELFCLASS64=2; 

      // Encoding
      ELFDATANONE=0; 
      ELFDATA2LSB=1; 
      ELFDATA2MSB=2; 

      // OS extensions
      ELFOSABI_NONE=0; // No extensions or unspecified
      ELFOSABI_HPUX=1; // Hewlett-Packard HP-UX
      ELFOSABI_NETBSD=2; // NetBSD
      ELFOSABI_LINUX=3; // Linux
      ELFOSABI_SOLARIS=6; // Sun Solaris
      ELFOSABI_AIX=7; // AIX
      ELFOSABI_IRIX=8; // IRIX
      ELFOSABI_FREEBSD=9; // FreeBSD
      ELFOSABI_TRU64=10; // Compaq TRU64 UNIX
      ELFOSABI_MODESTO=11; // Novell Modesto
      ELFOSABI_OPENBSD=12; // Open BSD
      ELFOSABI_OPENVMS=13; // Open VMS
      ELFOSABI_NSK=14; // Hewlett-Packard Non-Stop Kernel
      ELFOSABI_AROS=15; // Amiga Research OS
      ELFOSABI_FENIXOS=16; // The FenixOS highly scalable multi-core OS
      //                       64-255 Architecture-specific value range

      /////////////////////
      // Sections constants

      // Section indexes
      SHN_UNDEF=0; 
      SHN_LORESERVE=$ff00; 
      SHN_LOPROC=$ff00; 
      SHN_HIPROC=$ff1f; 
      SHN_LOOS=$ff20; 
      SHN_HIOS=$ff3f; 
      SHN_ABS=$fff1; 
      SHN_COMMON=$fff2; 
      SHN_XINDEX=$ffff; 
      SHN_HIRESERVE=$ffff; 

      // Section types
      SHT_NULL=0; 
      SHT_PROGBITS=1; 
      SHT_SYMTAB=2;
      SHT_STRTAB=3; 
      SHT_RELA=4;
      SHT_HASH=5; 
      SHT_DYNAMIC=6; 
      SHT_NOTE=7; 
      SHT_NOBITS=8; 
      SHT_REL=9; 
      SHT_SHLIB=10; 
      SHT_DYNSYM=11; 
      SHT_INIT_ARRAY=14; 
      SHT_FINI_ARRAY=15;
      SHT_PREINIT_ARRAY=16;
      SHT_GROUP=17; 
      SHT_SYMTAB_SHNDX=18; 
      SHT_LOOS=$60000000; 
      SHT_HIOS=$6fffffff;
      SHT_LOPROC=$70000000;
      SHT_HIPROC=$7fffffff;
      SHT_LOUSER=$80000000; 
      SHT_HIUSER=$ffffffff; 

      // Section attribute flags
      SHF_WRITE=$1; 
      SHF_ALLOC=$2; 
      SHF_EXECINSTR=$4; 
      SHF_MERGE=$10; 
      SHF_STRINGS=$20; 
      SHF_INFO_LINK=$40; 
      SHF_LINK_ORDER=$80; 
      SHF_OS_NONCONFORMING=$100; 
      SHF_GROUP=$200; 
      SHF_TLS=$400;
      SHF_MASKOS=$0ff00000;
      SHF_PRIVATE=$80000000; 
      SHF_MASKPROC=$f0000000; 

      // Section group flags
      GRP_COMDAT=$1; 
      GRP_MASKOS=$0ff00000; 
      GRP_MASKPROC=$f0000000; 

      // Symbol binding
      STB_LOCAL=0; 
      STB_GLOBAL=1;
      STB_WEAK=2; 
      STB_LOOS=10; 
      STB_HIOS=12; 
      STB_MULTIDEF=13;
      STB_LOPROC=13;
      STB_HIPROC=15; 

      // Symbol types
      STT_NOTYPE=0; 
      STT_OBJECT=1; 
      STT_FUNC=2;
      STT_SECTION=3; 
      STT_FILE=4; 
      STT_COMMON=5; 
      STT_TLS=6; 
      STT_LOOS=10; 
      STT_HIOS=12; 
      STT_LOPROC=13; 
      STT_HIPROC=15; 

      // Symbol visibility
      STV_DEFAULT=0; 
      STV_INTERNAL=1; 
      STV_HIDDEN=2; 
      STV_PROTECTED=3; 

      // Undefined name
      STN_UNDEF=0; 

      // Relocation types
      R_386_NONE=0; 
      R_X86_64_NONE=0;
      R_386_32=1; 
      R_X86_64_64=1; 
      R_386_PC32=2; 
      R_X86_64_PC32=2; 
      R_386_GOT32=3;
      R_X86_64_GOT32=3; 
      R_386_PLT32=4; 
      R_X86_64_PLT32=4;
      R_386_COPY=5; 
      R_X86_64_COPY=5; 
      R_386_GLOB_DAT=6; 
      R_X86_64_GLOB_DAT=6; 
      R_386_JMP_SLOT=7; 
      R_X86_64_JUMP_SLOT=7; 
      R_386_RELATIVE=8;
      R_X86_64_RELATIVE=8; 
      R_386_GOTOFF=9; 
      R_X86_64_GOTPCREL=9; 
      R_386_GOTPC=10; 
      R_X86_64_32=10;
      R_X86_64_32S=11;
      R_X86_64_16=12; 
      R_X86_64_PC16=13; 
      R_X86_64_8=14; 
      R_X86_64_PC8=15; 
      R_X86_64_DTPMOD64=16; 
      R_X86_64_DTPOFF64=17; 
      R_X86_64_TPOFF64=18; 
      R_X86_64_TLSGD=19; 
      R_X86_64_TLSLD=20; 
      R_X86_64_DTPOFF32=21; 
      R_X86_64_GOTTPOFF=22;
      R_X86_64_TPOFF32=23; 
      R_X86_64_PC64=24; 
      R_X86_64_GOTOFF64=25; 
      R_X86_64_GOTPC32=26; 
      R_X86_64_GOT64=27;
      R_X86_64_GOTPCREL64=28; 
      R_X86_64_GOTPC64=29; 
      R_X86_64_GOTPLT64=30; 
      R_X86_64_PLTOFF64=31; 
      R_X86_64_GOTPC32_TLSDESC=34; 
      R_X86_64_TLSDESC_CALL=35; 
      R_X86_64_TLSDESC=36;
      R_X86_64_IRELATIVE=37; 
      R_X86_64_GNU_VTINHERIT=250; 
      R_X86_64_GNU_VTENTRY=251; 

      // Segment types
      PT_NULL=0; 
      PT_LOAD=1;
      PT_DYNAMIC=2;
      PT_INTERP=3;
      PT_NOTE=4; 
      PT_SHLIB=5; 
      PT_PHDR=6; 
      PT_TLS=7; 
      PT_LOOS=$60000000; 
      PT_HIOS=$6fffffff; 
      PT_LOPROC=$70000000; 
      PT_HIPROC=$7fffffff; 

      // Segment flags
      PF_X=1; // Execute
      PF_W=2; // Write
      PF_R=4; // Read
      PF_MASKOS=$0ff00000; // Unspecified
      PF_MASKPROC=$f0000000; // Unspecified

      // Dynamic Array Tags
      DT_NULL=0; 
      DT_NEEDED=1;
      DT_PLTRELSZ=2; 
      DT_PLTGOT=3; 
      DT_HASH=4; 
      DT_STRTAB=5; 
      DT_SYMTAB=6; 
      DT_RELA=7; 
      DT_RELASZ=8; 
      DT_RELAENT=9;
      DT_STRSZ=10; 
      DT_SYMENT=11; 
      DT_INIT=12; 
      DT_FINI=13;
      DT_SONAME=14;
      DT_RPATH=15; 
      DT_SYMBOLIC=16;
      DT_REL=17; 
      DT_RELSZ=18; 
      DT_RELENT=19; 
      DT_PLTREL=20; 
      DT_DEBUG=21; 
      DT_TEXTREL=22; 
      DT_JMPREL=23; 
      DT_BIND_NOW=24; 
      DT_INIT_ARRAY=25;
      DT_FINI_ARRAY=26; 
      DT_INIT_ARRAYSZ=27; 
      DT_FINI_ARRAYSZ=28; 
      DT_RUNPATH=29; 
      DT_FLAGS=30;
      DT_ENCODING=32; 
      DT_PREINIT_ARRAY=32; 
      DT_PREINIT_ARRAYSZ=33;
      DT_MAXPOSTAGS=34; 
      DT_LOOS=$6000000d; 
      DT_HIOS=$6ffff000; 
      DT_LOPROC=$70000000; 
      DT_HIPROC=$7fffffff; 

      // DT_FLAGS values
      DF_ORIGIN=$1; 
      DF_SYMBOLIC=$2; 
      DF_TEXTREL=$4; 
      DF_BIND_NOW=$8; 
      DF_STATIC_TLS=$10;

      VER_DEF_NONE=0; // No version
      VER_DEF_CURRENT=1; // Current version
      VER_DEF_NUM=2; // Given version number

      // Legal values for vd_flags (version information flags)
      VER_FLG_BASE=1; // Version definition of file itself
      VER_FLG_WEAK=2; // Weak version identifier

      // Legal values for vn_version (version revision)
      VER_NEED_NONE=0; // No version
      VER_NEED_CURRENT=1; // Current version
      VER_NEED_NUM=2; // Given version number

      // Legal values for a_type (entry type)
      AT_NULL=0; // End of vector
      AT_IGNORE=1; // Entry should be ignored
      AT_EXECFD=2; // File descriptor of program
      AT_PHDR=3; // Program headers for program
      AT_PHENT=4; // Size of program header entry
      AT_PHNUM=5; // Number of program headers
      AT_PAGESZ=6; // System page size
      AT_BASE=7; // Base address of interpreter
      AT_FLAGS=8; // Flags
      AT_ENTRY=9; // Entry point of program
      AT_NOTELF=10; // Program is not ELF
      AT_UID=11; // Real uid
      AT_EUID=12; // Effective uid
      AT_GID=13; // Real gid
      AT_EGID=14; // Effective gid

      // Some more special a_type values describing the hardware.
      AT_PLATFORM=15; // String identifying platform
      AT_HWCAP=16; // Machine dependent hints about processor capabilities

      // This entry gives some information about the FPU initialization performed by the kernel.
      AT_FPUCW=17; //  Used FPU control word.

type PELFHalf=^TELFHalf;
     TELFHalf=TPACCUInt16;

     PELFWord=^TELFWord;
     TELFWord=TPACCUInt32;

     PELFSWord=^TELFSWord;
     TELFSWord=TPACCInt32;

     PELFXWord=^TELFXWord;
     TELFXWord=TPACCUInt64;

     PELFSXWord=^TELFSXWord;
     TELFSXWord=TPACCInt64;

     PELF32Addr=^TELF32Addr;
     TELF32Addr=TPACCUInt32;

     PELF32Off=^TELF32Off;
     TELF32Off=TPACCUInt32;

     PELF64Addr=^TELF32Addr;
     TELF64Addr=TPACCUInt64;

     PELF64Off=^TELF32Off;
     TELF64Off=TPACCUInt64;

     PELF32Half=^TELF32Half;
     TELF32Half=TELFHalf;

     PELF64Half=^TELF64Half;
     TELF64Half=TELFHalf;

     PELF32Word=^TELF32Word;
     TELF32Word=TELFWord;

     PELF64Word=^TELF64Word;
     TELF64Word=TELFWord;

     PELF32SWord=^TELF32SWord;
     TELF32SWord=TELFSWord;

     PELF64SWord=^TELF64SWord;
     TELF64SWord=TELFSWord;

type PELFIdent=^TELFIdent;
     TELFIdent=array[0..EI_NIDENT-1] of TPACCUInt8;

     // File header
     PELF32EHdr=^TELF32EHdr;
     TELF32EHdr=packed record
      e_ident:TELFIdent;
      e_type:TELFHalf;
      e_machine:TELFHalf;
      e_version:TELFWord;
      e_entry:TELF32Addr;
      e_phoff:TELF32Off;
      e_shoff:TELF32Off;
      e_flags:TELFWord;
      e_ehsize:TELFHalf;
      e_phentsize:TELFHalf;
      e_phnum:TELFHalf;
      e_shentsize:TELFHalf;
      e_shnum:TELFHalf;
      e_shstrndx:TELFHalf;
     end;

     PELF64EHdr=^TELF64EHdr;
     TELF64EHdr=packed record
      e_ident:TELFIdent;
      e_type:TELFHalf;
      e_machine:TELFHalf;
      e_version:TELFWord;
      e_entry:TELF64Addr;
      e_phoff:TELF64Off;
      e_shoff:TELF64Off;
      e_flags:TELFWord;
      e_ehsize:TELFHalf;
      e_phentsize:TELFHalf;
      e_phnum:TELFHalf;
      e_shentsize:TELFHalf;
      e_shnum:TELFHalf;
      e_shstrndx:TELFHalf;
     end;

     PELFCommonEHdr=^TELFCommonEHdr;
     TELFCommonEHdr=packed record
      e_ident:TELFIdent;
      e_type:TELFHalf;
      e_machine:TELFHalf;
      e_version:TELFWord;
     end;

     PELF3264EHdr=^TELF3264EHdr;
     TELF3264EHdr=packed record
      case TPACCInt of
       0:(
        CommonEHdr:TELFCommonEHdr;
        AfterCommonEHdr:pointer;
       );
       32:(
        ELF32EHdr:TELF32EHdr;
       );
       64:(
        ELF64EHdr:TELF64EHdr;
       );
     end;

     // Section header
     PELF32SHdr=^TELF32SHdr;
     TELF32SHdr=packed record
      sh_name:TELFWord;
      sh_type:TELFWord;
      sh_flags:TELFWord;
      sh_addr:TELF32Addr;
      sh_offset:TELF32Off;
      sh_size:TELFWord;
      sh_link:TELFWord;
      sh_info:TELFWord;
      sh_addralign:TELFWord;
      sh_entsize:TELFWord;
     end;

     PELF64SHdr=^TELF64SHdr;
     TELF64SHdr=packed record
      sh_name:TELFWord;
      sh_type:TELFWord;
      sh_flags:TELFXWord;
      sh_addr:TELF64Addr;
      sh_offset:TELF64Off;
      sh_size:TELFXWord;
      sh_link:TELFWord;
      sh_info:TELFWord;
      sh_addralign:TELFXWord;
      sh_entsize:TELFXWord;
     end;

     PELFCommonSHdr=^TELFCommonSHdr;
     TELFCommonSHdr=packed record
      sh_name:TELFWord;
      sh_type:TELFWord;
     end;

     PELF3264SHdr=^TELF3264SHdr;
     TELF3264SHdr=packed record
      case TPACCInt of
       0:(
        CommonSHdr:TELFCommonSHdr;
        AfterCommonSHdr:pointer;
       );
       32:(
        ELF32SHdr:TELF32SHdr;
       );
       64:(
        ELF64SHdr:TELF64SHdr;
       );
     end;

     // Segment header
     PELF32PHdr=^TELF32PHdr;
     TELF32PHdr=packed record
      p_type:TELFWord;
      p_offset:TELF32Off;
      p_vaddr:TELF32Addr;
      p_paddr:TELF32Addr;
      p_filesz:TELFWord;
      p_memsz:TELFWord;
      p_flags:TELFWord;
      p_align:TELFWord;
     end;

     PELF64PHdr=^TELF64PHdr;
     TELF64PHdr=packed record
      p_type:TELFWord;
      p_flags:TELFWord;
      p_offset:TELF64Off;
      p_vaddr:TELF64Addr;
      p_paddr:TELF64Addr;
      p_filesz:TELFXWord;
      p_memsz:TELFXWord;
      p_align:TELFXWord;
     end;

     PELFCommonPHdr=^TELFCommonPHdr;
     TELFCommonPHdr=packed record
      p_type:TELFWord;
     end;

     PELF3264PHdr=^TELF3264PHdr;
     TELF3264PHdr=packed record
      case TPACCInt of
       0:(
        CommonPHdr:TELFCommonPHdr;
        AfterCommonPHdr:pointer;
       );
       32:(
        ELF32PHdr:TELF32PHdr;
       );
       64:(
        ELF64PHdr:TELF64PHdr;
       );
     end;

     // Symbol table entry
     PELF32Sym=^TELF32Sym;
     TELF32Sym=packed record
      st_name:TELFWord;
      st_value:TELF32Addr;
      st_size:TELFWord;
      st_info:TPACCUInt8;
      st_other:TPACCUInt8;
      st_shndx:TELFHalf;
     end;

     PELF64Sym=^TELF64Sym;
     TELF64Sym=packed record
      st_name:TELFWord;
      st_info:TPACCUInt8;
      st_other:TPACCUInt8;
      st_shndx:TELFHalf;
      st_value:TELF64Addr;
      st_size:TELFXWord;
     end;

     PELFCommonSym=^TELFCommonSym;
     TELFCommonSym=packed record
      st_name:TELFWord;
     end;

     PELF3264Sym=^TELF3264Sym;
     TELF3264Sym=packed record
      case TPACCInt of
       0:(
        CommonSym:TELFCommonSym;
        AfterCommonSym:pointer;
       );
       32:(
        ELF32Sym:TELF32Sym;
       );
       64:(
        ELF64Sym:TELF64Sym;
       );
     end;

     // Relocation entries
     PELF32Rel=^TELF32Rel;
     TELF32Rel=packed record
      r_offset:TELF32Addr;
      r_info:TELFWord;
     end;

     PELF32Rela=^TELF32Rela;
     TELF32Rela=packed record
      r_offset:TELF32Addr;
      r_info:TELFWord;
      r_addend:TELFSWord;
     end;

     PELF64Rel=^TELF64Rel;
     TELF64Rel=packed record
      r_offset:TELF64Addr;
      r_info:TELFXWord;
     end;

     PELF64Rela=^TELF64Rela;
     TELF64Rela=packed record
      r_offset:TELF64Addr;
      r_info:TELFXWord;
      r_addend:TELFSXWord;
     end;

     PELF3264Rel=^TELF3264Rel;
     TELF3264Rel=packed record
      case TPACCInt of
       32:(
        ELF32Rel:TELF32Rel;
       );
       64:(
        ELF64Rel:TELF64Rel;
       );
     end;

     PELF3264Rela=^TELF3264Rela;
     TELF3264Rela=packed record
      case TPACCInt of
       32:(
        ELF32Rela:TELF32Rela;
       );
       64:(
        ELF64Rela:TELF64Rela;
       );
     end;

     // Dynamic structure
     PELF32Dyn=^TELF32Dyn;
     TELF32Dyn=packed record
      case d_tag:TELFSWord of
       0:(
        d_val:TELFWord;
       );
       1:(
        d_ptr:TELF32Addr;
       );
     end;

     PELF64Dyn=^TELF64Dyn;
     TELF64Dyn=packed record
      case d_tag:TELFSXWord of
       0:(
        d_val:TELFXWord;
       );
       1:(
        d_ptr:TELF64Addr;
       );
     end;

     // Version definition sections
     PELF32Verdef=^TELF32Verdef;
     TELF32Verdef=packed record
      vd_version:TELF32Half;
      vd_flags:TELF32Half;
      vd_ndx:TELF32Half;
      vd_cnt:TELF32Half;
      vd_hash:TELF32Word;
      vd_aux:TELF32Word;
      vd_next:TELF32Word;
     end;

     PELF64Verdef=^TELF64Verdef;
     TELF64Verdef=packed record
      vd_version:TELF64Half;
      vd_flags:TELF64Half;
      vd_ndx:TELF64Half;
      vd_cnt:TELF64Half;
      vd_hash:TELF64Word;
      vd_aux:TELF64Word;
      vd_next:TELF64Word;
     end;

     // Version dependency section
     PELF32Verneed=^TELF32Verneed;
     TELF32Verneed=packed record
      vn_version:TELF32Half;
      vn_cnt:TELF32Half;
      vn_file:TELF32Word;
      vn_aux:TELF32Word;
      vn_next:TELF32Word;
     end;

     PELF64Verneed=^TELF64Verneed;
     TELF64Verneed=packed record
      vn_version:TELF64Half;
      vn_cnt:TELF64Half;
      vn_file:TELF64Word;
      vn_aux:TELF64Word;
      vn_next:TELF64Word;
     end;

     // Auxiliary needed version information
     PELF32Vernaux=^TELF32Vernaux;
     TELF32Vernaux=packed record
      vna_hash:TELF32Word;
      vna_flahs:TELF32Half;
      vna_other:TELF32Word;
      vna_name:TELF32Word;
      vna_next:TELF32Word;
     end;

     PELF64Vernaux=^TELF64Vernaux;
     TELF64Vernaux=packed record
      vna_hash:TELF64Word;
      vna_flahs:TELF64Half;
      vna_other:TELF64Word;
      vna_name:TELF64Word;
      vna_next:TELF64Word;
     end;

     // Auxiliary vector
     PELF32AuxV=^TELF32AuxV;
     TELF32AuxV=packed record
      case a_int:TPACCInt32 of
       0:(
        a_val:TPACCInt64;
       );
       1:(
        a_ptr:pointer;
       );
     end;

     PELF64AuxV=^TELF64AuxV;
     TELF64AuxV=packed record
      case a_int:TPACCInt64 of
       0:(
        a_val:TPACCInt64;
       );
       1:(
        a_ptr:pointer;
       );
     end;

     TPACCLinker_ELF_ELF=class;

     TPACCLinker_ELF_ELF_Import=class
      public
       Used:boolean;
       SymbolName:TPUCUUTF8String;
       ImportLibraryName:TPUCUUTF8String;
       ImportName:TPUCUUTF8String;
       CodeSectionOffset:TPACCUInt64;
       NameOffset:TPACCUInt64;
     end;

     TPACCLinker_ELF_ELF_ImportList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Import;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Import);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Import read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF_Export=class
      public
       Used:boolean;
       SymbolName:TPUCUUTF8String;
       ExportName:TPUCUUTF8String;
     end;

     TPACCLinker_ELF_ELF_ExportList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Export;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Export);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Export read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF_Section=class;

     TPACCLinker_ELF_ELF_Symbol=class
      private

       fLinker:TPACCLinker_ELF_ELF;

       fIndex:TPACCInt32;

       fName:TPACCRawByteString;

       fSection:TPACCLinker_ELF_ELF_Section;

       fst_name:TPACCUInt64;
       fst_info:TPACCUInt64;
       fst_other:TPACCUInt64;
       fst_shndx:TPACCUInt64;
       fst_value:TPACCUInt64;
       fst_size:TPACCUInt64;

       fMergedSymbol:TPACCLinker_ELF_ELF_Symbol;

       fActive:boolean;

      public

       constructor Create(const ALinker:TPACCLinker_ELF_ELF); reintroduce;
       destructor Destroy; override;

      published

       property Linker:TPACCLinker_ELF_ELF read fLinker;

       property Index_:TPACCInt32 read fIndex write fIndex;

       property Name:TPACCRawByteString read fName write fName;

       property Section:TPACCLinker_ELF_ELF_Section read fSection write fSection;

       property st_name:TPACCUInt64 read fst_name write fst_name;
       property st_info:TPACCUInt64 read fst_info write fst_info;
       property st_other:TPACCUInt64 read fst_other write fst_other;
       property st_shndx:TPACCUInt64 read fst_shndx write fst_shndx;
       property st_value:TPACCUInt64 read fst_value write fst_value;
       property st_size:TPACCUInt64 read fst_size write fst_size;

       property MergedSymbol:TPACCLinker_ELF_ELF_Symbol read fMergedSymbol write fMergedSymbol;

       property Active:boolean read fActive write fActive;

     end;

     TPACCLinker_ELF_ELF_SymbolList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Symbol;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Symbol);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Symbol read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF_Relocation=class
      private

       fLinker:TPACCLinker_ELF_ELF;

       fName:TPACCRawByteString;

       fSection:TPACCLinker_ELF_ELF_Section;

       fSymbol:TPACCLinker_ELF_ELF_Symbol;

       fr_offset:TELF64Addr;
       fr_info:TELFXWord;
       fr_addend:TELFSXWord;

       fActive:boolean;

      public

       constructor Create(const ALinker:TPACCLinker_ELF_ELF); reintroduce;
       destructor Destroy; override;

      published

       property Linker:TPACCLinker_ELF_ELF read fLinker;

       property Name:TPACCRawByteString read fName write fName;

       property Section:TPACCLinker_ELF_ELF_Section read fSection write fSection;

       property Symbol:TPACCLinker_ELF_ELF_Symbol read fSymbol write fSymbol;

       property r_offset:TELF64Addr read fr_offset write fr_offset;
       property r_info:TELFXWord read fr_info write fr_info;
       property r_addend:TELFSXWord read fr_addend write fr_addend;

       property Active:boolean read fActive write fActive;

     end;

     TPACCLinker_ELF_ELF_RelocationList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Relocation;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Relocation);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Relocation read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF_Section=class
      private

       fLinker:TPACCLinker_ELF_ELF;

       fIndex:TPACCInt32;

       fName:TPACCRawByteString;

       fDataOffset:TPACCUInt64;

       fStream:TMemoryStream;

       fsh_name:TPACCUInt64;
       fsh_num:TPACCUInt64;
       fsh_type:TPACCUInt64;
       fsh_flags:TPACCUInt64;
       fsh_addr:TPACCUInt64;
       fsh_offset:TPACCUInt64;
       fsh_size:TPACCUInt64;
       fsh_link:TPACCUInt64;
       fsh_info:TPACCUInt64;
       fsh_addralign:TPACCUInt64;
       fsh_entsize:TPACCUInt64;

       fSymbols:TPACCLinker_ELF_ELF_SymbolList;

       fRelocations:TPACCLinker_ELF_ELF_RelocationList;

       fMergedOffset:TPACCInt64;

       fMergedToSection:TPACCLinker_ELF_ELF_Section;

       fLinkSection:TPACCLinker_ELF_ELF_Section;

       fInfoSection:TPACCLinker_ELF_ELF_Section;

       fMerged:boolean;

       fLinkOnce:boolean;

       fActive:boolean;

      public

       constructor Create(const ALinker:TPACCLinker_ELF_ELF); reintroduce;
       destructor Destroy; override;

      published

       property Linker:TPACCLinker_ELF_ELF read fLinker;

       property Index_:TPACCInt32 read fIndex write fIndex;

       property Name:TPACCRawByteString read fName write fName;

       property DataOffset:TPACCUInt64 read fDataOffset write fDataOffset;

       property Stream:TMemoryStream read fStream write fStream;

       property sh_name:TPACCUInt64 read fsh_name write fsh_name;
       property sh_num:TPACCUInt64 read fsh_num write fsh_num;
       property sh_type:TPACCUInt64 read fsh_type write fsh_type;
       property sh_flags:TPACCUInt64 read fsh_flags write fsh_flags;
       property sh_addr:TPACCUInt64 read fsh_addr write fsh_addr;
       property sh_offset:TPACCUInt64 read fsh_offset write fsh_offset;
       property sh_size:TPACCUInt64 read fsh_size write fsh_size;
       property sh_link:TPACCUInt64 read fsh_link write fsh_link;
       property sh_info:TPACCUInt64 read fsh_info write fsh_info;
       property sh_addralign:TPACCUInt64 read fsh_addralign write fsh_addralign;
       property sh_entsize:TPACCUInt64 read fsh_entsize write fsh_entsize;

       property Symbols:TPACCLinker_ELF_ELF_SymbolList read fSymbols;

       property Relocations:TPACCLinker_ELF_ELF_RelocationList read fRelocations;

       property MergedOffset:TPACCInt64 read fMergedOffset write fMergedOffset;

       property MergedToSection:TPACCLinker_ELF_ELF_Section read fMergedToSection write fMergedToSection;

       property LinkSection:TPACCLinker_ELF_ELF_Section read fLinkSection write fLinkSection;

       property InfoSection:TPACCLinker_ELF_ELF_Section read fInfoSection write fInfoSection;

       property Merged:boolean read fMerged write fMerged;

       property LinkOnce:boolean read fLinkOnce write fLinkOnce;

       property Active:boolean read fActive write fActive;

     end;

     TPACCLinker_ELF_ELF_SectionList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Section;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Section);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Section read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF_Image=class
      private

       fLinker:TPACCLinker_ELF_ELF;

       fName:TPACCRawByteString;

       fSections:TPACCLinker_ELF_ELF_SectionList;

       fSymbols:TPACCLinker_ELF_ELF_SymbolList;

       fSHStrTabSection:TPACCLinker_ELF_ELF_Section;

       fStrTabSection:TPACCLinker_ELF_ELF_Section;

       fSymTabSection:TPACCLinker_ELF_ELF_Section;

       fActive:boolean;

      public

       constructor Create(const ALinker:TPACCLinker_ELF_ELF); reintroduce;
       destructor Destroy; override;

      published

       property Linker:TPACCLinker_ELF_ELF read fLinker;

       property Name:TPACCRawByteString read fName write fName;

       property Sections:TPACCLinker_ELF_ELF_SectionList read fSections write fSections;

       property Symbols:TPACCLinker_ELF_ELF_SymbolList read fSymbols write fSymbols;

       property SHStrTabSection:TPACCLinker_ELF_ELF_Section read fSHStrTabSection write fSHStrTabSection;

       property StrTabSection:TPACCLinker_ELF_ELF_Section read fStrTabSection write fStrTabSection;

       property SymTabSection:TPACCLinker_ELF_ELF_Section read fSymTabSection write fSymTabSection;

       property Active:boolean read fActive write fActive;

     end;

     TPACCLinker_ELF_ELF_ImageList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Image;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Image);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_ELF_ELF_Image read GetItem write SetItem; default;
     end;

     TPACCLinker_ELF_ELF=class(TPACCLinker)
      private

       fIs64Bit:boolean;

       fMachine:TPACCInt32;

       fImages:TPACCLinker_ELF_ELF_ImageList;

       fImports:TPACCLinker_ELF_ELF_ImportList;
       fImportSymbolNameHashMap:TPACCRawByteStringHashMap;

       fExports:TPACCLinker_ELF_ELF_ExportList;
       fExportSymbolNameHashMap:TPACCRawByteStringHashMap;

      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       procedure AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String); override;

       procedure AddExport(const ASymbolName,AExportName:TPUCUUTF8String); override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); override;

       procedure AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); override;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

      published

       property Is64Bit:boolean read fIs64Bit write fIs64Bit;

       property Machine:TPACCInt32 read fMachine write fMachine;

       property Images:TPACCLinker_ELF_ELF_ImageList read fImages;

       property Imports_:TPACCLinker_ELF_ELF_ImportList read fImports;

       property Exports_:TPACCLinker_ELF_ELF_ExportList read fExports;

     end;

function ELF_ST_BIND(const i:TELFWord):TELFWord;
function ELF_ST_TYPE(const i:TELFWord):TELFWord;
function ELF_ST_INFO(const b,t:TELFWord):TELFWord;

function ELF_ST_VISIBILITY(const o:TELFWord):TELFWord;

function ELF32_R_SYM(const i:TELFWord):TELFWord;
function ELF32_R_TYPE(const i:TELFWord):TPACCUInt8;
function ELF32_R_INFO(const s,t:TELFWord):TELFWord;

function ELF64_R_SYM(const i:TELFXWord):TELFWord;
function ELF64_R_TYPE(const i:TELFXWord):TELFWord;
function ELF64_R_INFO(const s,t:TELFXWord):TELFXWord;

function ELF_HASH(Name:PAnsiChar):TPACCUInt64;

implementation

uses PACCInstance,PACCTarget_x86_32;

type PSTabSym=^TSTabSym;
     TSTabSym=packed record
      n_strx:TPACCUInt32; // index into string table of name
      n_type:TPACCUInt8; // type of symbol
      n_other:TPACCUInt8; // misc info (usually empty)
      n_desc:TPACCUInt16; // description field
      n_value:TPACCUInt32; // value of symbol
     end;

function ELF_ST_BIND(const i:TELFWord):TELFWord;
begin
 result:=i shr 4;
end;

function ELF_ST_TYPE(const i:TELFWord):TELFWord;
begin
 result:=i and $f;
end;

function ELF_ST_INFO(const b,t:TELFWord):TELFWord;
begin
 result:=(b shl 4) or (t and $f);
end;

function ELF_ST_VISIBILITY(const o:TELFWord):TELFWord;
begin
 result:=o and 3;
end;

function ELF32_R_SYM(const i:TELFWord):TELFWord;
begin
 result:=i shr 8;
end;

function ELF32_R_TYPE(const i:TELFWord):TPACCUInt8;
begin
 result:=i and $ff;
end;

function ELF32_R_INFO(const s,t:TELFWord):TELFWord;
begin
 result:=(s shl 8) or (t and $ff);
end;

function ELF64_R_SYM(const i:TELFXWord):TELFWord;
begin
 result:=i shr 32;
end;

function ELF64_R_TYPE(const i:TELFXWord):TELFWord;
begin
 result:=i and $ffffffff;
end;

function ELF64_R_INFO(const s,t:TELFXWord):TELFXWord;
begin
 result:=(s shl 32) or (t and $ffffffff);
end;

function ELF_HASH(Name:PAnsiChar):TPACCUInt64;
var t:TPACCUInt64;
begin
 result:=0;
 while assigned(Name) and (Name^<>#0) do begin
  result:=(result shl 4)+TPACCUInt8(AnsiChar(Name^));
  inc(Name);
  t:=result and $f0000000;
  if t<>0 then begin
   result:=result xor (t shr 24);
  end;
  result:=result and not t;
 end;
end;

constructor TPACCLinker_ELF_ELF_ImportList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_ImportList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_ImportList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Import;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_ImportList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Import);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF_ExportList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_ExportList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_ExportList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Export;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_ExportList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Export);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF_Symbol.Create(const ALinker:TPACCLinker_ELF_ELF);
begin
 inherited Create;

 fLinker:=ALinker;

 fName:='';

 fSection:=nil;

 fMergedSymbol:=nil;

 fActive:=true;

end;

destructor TPACCLinker_ELF_ELF_Symbol.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCLinker_ELF_ELF_SymbolList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_SymbolList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_SymbolList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Symbol;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_SymbolList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Symbol);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF_Relocation.Create(const ALinker:TPACCLinker_ELF_ELF);
begin
 inherited Create;

 fLinker:=ALinker;

 fName:='';

 fSection:=nil;

 fSymbol:=nil;

 fActive:=true;

end;

destructor TPACCLinker_ELF_ELF_Relocation.Destroy;
begin
 inherited Destroy;
end;

constructor TPACCLinker_ELF_ELF_RelocationList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_RelocationList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_RelocationList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Relocation;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_RelocationList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Relocation);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF_Section.Create(const ALinker:TPACCLinker_ELF_ELF);
var Index:TPACCInt32;
begin
 inherited Create;

 fLinker:=ALinker;

 fName:='';

 fStream:=TMemoryStream.Create;

 fSymbols:=TPACCLinker_ELF_ELF_SymbolList.Create;

 fRelocations:=TPACCLinker_ELF_ELF_RelocationList.Create;

 fActive:=true;

end;

destructor TPACCLinker_ELF_ELF_Section.Destroy;
begin

 fStream.Free;

 fSymbols.Free;

 fRelocations.Free;

 inherited Destroy;
end;

constructor TPACCLinker_ELF_ELF_SectionList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_SectionList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_SectionList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Section;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_SectionList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Section);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF_Image.Create(const ALinker:TPACCLinker_ELF_ELF);
begin
 inherited Create;

 fLinker:=ALinker;

 fName:='';

 fSections:=TPACCLinker_ELF_ELF_SectionList.Create;

 fSymbols:=TPACCLinker_ELF_ELF_SymbolList.Create;

 fActive:=true;

end;

destructor TPACCLinker_ELF_ELF_Image.Destroy;
begin

 while fSymbols.Count>0 do begin
  fSymbols[fSymbols.Count-1].Free;
  fSymbols.Delete(fSymbols.Count-1);
 end;
 fSymbols.Free;

 while fSections.Count>0 do begin
  fSections[fSections.Count-1].Free;
  fSections.Delete(fSections.Count-1);
 end;
 fSections.Free;

 inherited Destroy;
end;

constructor TPACCLinker_ELF_ELF_ImageList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_ELF_ELF_ImageList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_ELF_ELF_ImageList.GetItem(const AIndex:TPACCInt):TPACCLinker_ELF_ELF_Image;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_ELF_ELF_ImageList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_ELF_ELF_Image);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_ELF_ELF.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);

 fIs64Bit:=false;

 if TPACCInstance(Instance).Target is TPACCTarget_x86_32_ELF_ELF then begin
  fMachine:=EM_386;
 end;

 fImages:=TPACCLinker_ELF_ELF_ImageList.Create;

 fImports:=TPACCLinker_ELF_ELF_ImportList.Create;
 fImportSymbolNameHashMap:=TPACCRawByteStringHashMap.Create;

 fExports:=TPACCLinker_ELF_ELF_ExportList.Create;
 fExportSymbolNameHashMap:=TPACCRawByteStringHashMap.Create;

end;

destructor TPACCLinker_ELF_ELF.Destroy;
begin

 fImages.Free;

 fImports.Free;
 fImportSymbolNameHashMap.Free;

 fExports.Free;
 fExportSymbolNameHashMap.Free;

 inherited Destroy;
end;

procedure TPACCLinker_ELF_ELF.AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String);
var Import_:TPACCLinker_ELF_ELF_Import;
begin
 if assigned(fImportSymbolNameHashMap[ASymbolName]) then begin
  TPACCInstance(Instance).AddWarning('Duplicate import symbol name "'+ASymbolName+'"',nil);
 end else begin
  Import_:=TPACCLinker_ELF_ELF_Import.Create;
  fImports.Add(Import_);
  Import_.Used:=false;
  Import_.SymbolName:=ASymbolName;
  Import_.ImportLibraryName:=AImportLibraryName;
  Import_.ImportName:=AImportName;
  fImportSymbolNameHashMap[ASymbolName]:=Import_;
 end;
end;

procedure TPACCLinker_ELF_ELF.AddExport(const ASymbolName,AExportName:TPUCUUTF8String);
var Export_:TPACCLinker_ELF_ELF_Export;
begin
 if assigned( fExportSymbolNameHashMap[ASymbolName]) then begin
  TPACCInstance(Instance).AddWarning('Duplicate export symbol name "'+ASymbolName+'"',nil);
 end else begin
  Export_:=TPACCLinker_ELF_ELF_Export.Create;
  fExports.Add(Export_);
  Export_.Used:=false;
  Export_.SymbolName:=ASymbolName;
  Export_.ExportName:=AExportName;
  fExportSymbolNameHashMap[ASymbolName]:=Export_;
 end;
end;

procedure TPACCLinker_ELF_ELF.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
var SectionIndex,SymTabSectionIndex,SymbolIndex:TPACCInt32;
    ELF3264EHdr:TELF3264EHdr;
    ELF3264SHdr:TELF3264SHdr;
    ELF3264Sym:TELF3264Sym;
    ELF3264Rel:TELF3264Rel;
    ELF3264Rela:TELF3264Rela;
    Image:TPACCLinker_ELF_ELF_Image;
    Section,TargetSection:TPACCLinker_ELF_ELF_Section;
    Symbol:TPACCLinker_ELF_ELF_Symbol;
    Relocation:TPACCLinker_ELF_ELF_Relocation;
    Name:TPACCRawByteString;
    c:AnsiChar;
begin

 if AObjectStream.Seek(0,soBeginning)<>0 then begin
  TPACCInstance(Instance).AddError('Stream seek error',nil,true);
 end;

 AObjectStream.ReadBuffer(ELF3264EHdr.CommonEHdr,SizeOf(TELFCommonEHdr));

 if (ELF3264EHdr.CommonEHdr.e_ident[0]<>ELFMAG0) or
    (ELF3264EHdr.CommonEHdr.e_ident[1]<>ELFMAG1) or
    (ELF3264EHdr.CommonEHdr.e_ident[2]<>ELFMAG2) or
    (ELF3264EHdr.CommonEHdr.e_ident[3]<>ELFMAG3) then begin
  TPACCInstance(Instance).AddError('No ELF object',nil,true);
 end;

 if ((not Is64Bit) and (ELF3264EHdr.CommonEHdr.e_ident[4]<>ELFCLASS32)) or
    (Is64Bit and (ELF3264EHdr.CommonEHdr.e_ident[4]<>ELFCLASS64)) then begin
  TPACCInstance(Instance).AddError('Wrong ELF bitness',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_ident[5]<>ELFDATA2LSB then begin
  TPACCInstance(Instance).AddError('Wrong ELF endianess',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_ident[6]<>EV_CURRENT then begin
  TPACCInstance(Instance).AddError('Wrong ELF version',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_ident[7]<>ELFOSABI_NONE then begin
  TPACCInstance(Instance).AddError('Wrong ELF ABI',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_type<>ET_REL then begin
  TPACCInstance(Instance).AddError('No ELF object',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_machine<>fMachine then begin
  TPACCInstance(Instance).AddError('Wrong ELF machine',nil,true);
 end;

 if ELF3264EHdr.CommonEHdr.e_version<>EV_CURRENT then begin
  TPACCInstance(Instance).AddError('Wrong ELF version',nil,true);
 end;

 if fIs64Bit then begin

  AObjectStream.ReadBuffer(ELF3264EHdr.AfterCommonEHdr,SizeOf(TELF64EHdr)-SizeOf(TELFCommonEHdr));

 end else begin

  AObjectStream.ReadBuffer(ELF3264EHdr.AfterCommonEHdr,SizeOf(TELF32EHdr)-SizeOf(TELFCommonEHdr));

  // Convert 32-bit ELF file header into a 64-bit ELF file header in-place by reversed field order,
  // because some 64-bit header fields are larger than these in the 32-bit header
  ELF3264EHdr.ELF64EHdr.e_shstrndx:=ELF3264EHdr.ELF32EHdr.e_shstrndx;
  ELF3264EHdr.ELF64EHdr.e_shnum:=ELF3264EHdr.ELF32EHdr.e_shnum;
  ELF3264EHdr.ELF64EHdr.e_shentsize:=ELF3264EHdr.ELF32EHdr.e_shentsize;
  ELF3264EHdr.ELF64EHdr.e_phnum:=ELF3264EHdr.ELF32EHdr.e_phnum;
  ELF3264EHdr.ELF64EHdr.e_phentsize:=ELF3264EHdr.ELF32EHdr.e_phentsize;
  ELF3264EHdr.ELF64EHdr.e_ehsize:=ELF3264EHdr.ELF32EHdr.e_ehsize;
  ELF3264EHdr.ELF64EHdr.e_flags:=ELF3264EHdr.ELF32EHdr.e_flags;
  ELF3264EHdr.ELF64EHdr.e_shoff:=ELF3264EHdr.ELF32EHdr.e_shoff;
  ELF3264EHdr.ELF64EHdr.e_phoff:=ELF3264EHdr.ELF32EHdr.e_phoff;
  ELF3264EHdr.ELF64EHdr.e_entry:=ELF3264EHdr.ELF32EHdr.e_entry;
  ELF3264EHdr.ELF64EHdr.e_version:=ELF3264EHdr.ELF32EHdr.e_version;
  ELF3264EHdr.ELF64EHdr.e_machine:=ELF3264EHdr.ELF32EHdr.e_machine;
  ELF3264EHdr.ELF64EHdr.e_type:=ELF3264EHdr.ELF32EHdr.e_type;
  ELF3264EHdr.ELF64EHdr.e_ident:=ELF3264EHdr.ELF32EHdr.e_ident;

 end;

 if AObjectStream.Seek(ELF3264EHdr.ELF64EHdr.e_shoff,soBeginning)<>ELF3264EHdr.ELF64EHdr.e_shoff then begin
  TPACCInstance(Instance).AddError('Stream seek error',nil,true);
 end;

 Image:=TPACCLinker_ELF_ELF_Image.Create(self);
 Images.Add(Image);

 Image.Name:=AObjectFileName;

 for SectionIndex:=0 to ELF3264EHdr.ELF64EHdr.e_shnum-1 do begin

  Section:=TPACCLinker_ELF_ELF_Section.Create(self);
  Section.Index_:=Image.Sections.Add(Section);

  if fIs64Bit then begin

   AObjectStream.ReadBuffer(ELF3264SHdr.ELF64SHdr,SizeOf(TELF64SHdr));

   Section.sh_name:=ELF3264SHdr.ELF64SHdr.sh_name;
   Section.sh_type:=ELF3264SHdr.ELF64SHdr.sh_type;
   Section.sh_flags:=ELF3264SHdr.ELF64SHdr.sh_flags;
   Section.sh_addr:=ELF3264SHdr.ELF64SHdr.sh_addr;
   Section.sh_offset:=ELF3264SHdr.ELF64SHdr.sh_offset;
   Section.sh_size:=ELF3264SHdr.ELF64SHdr.sh_size;
   Section.sh_link:=ELF3264SHdr.ELF64SHdr.sh_link;
   Section.sh_info:=ELF3264SHdr.ELF64SHdr.sh_info;
   Section.sh_addralign:=ELF3264SHdr.ELF64SHdr.sh_addralign;
   Section.sh_entsize:=ELF3264SHdr.ELF64SHdr.sh_entsize;

  end else begin

   AObjectStream.ReadBuffer(ELF3264SHdr.ELF32SHdr,SizeOf(TELF32SHdr));

   Section.sh_name:=ELF3264SHdr.ELF32SHdr.sh_name;
   Section.sh_type:=ELF3264SHdr.ELF32SHdr.sh_type;
   Section.sh_flags:=ELF3264SHdr.ELF32SHdr.sh_flags;
   Section.sh_addr:=ELF3264SHdr.ELF32SHdr.sh_addr;
   Section.sh_offset:=ELF3264SHdr.ELF32SHdr.sh_offset;
   Section.sh_size:=ELF3264SHdr.ELF32SHdr.sh_size;
   Section.sh_link:=ELF3264SHdr.ELF32SHdr.sh_link;
   Section.sh_info:=ELF3264SHdr.ELF32SHdr.sh_info;
   Section.sh_addralign:=ELF3264SHdr.ELF32SHdr.sh_addralign;
   Section.sh_entsize:=ELF3264SHdr.ELF32SHdr.sh_entsize;

  end;

 end;

 for SectionIndex:=0 to Image.Sections.Count-1 do begin
  Section:=Image.Sections[SectionIndex];
  if Section.sh_size>0 then begin
   if AObjectStream.Seek(Section.sh_offset,soBeginning)<>Section.sh_offset then begin
    TPACCInstance(Instance).AddError('Stream seek error',nil,true);
   end;
   if Section.Stream.CopyFrom(AObjectStream,Section.sh_size)<>Section.sh_size then begin
    TPACCInstance(Instance).AddError('Stream read error',nil,true);
   end;
  end;
 end;

 if ELF3264EHdr.ELF64EHdr.e_shstrndx<Image.Sections.Count then begin
  Image.SHStrTabSection:=Image.Sections[ELF3264EHdr.ELF64EHdr.e_shstrndx];
 end else begin
  Image.SHStrTabSection:=nil;
  TPACCInstance(Instance).AddError('No ".shstrtab" section',nil,true);
 end;

 Image.StrTabSection:=nil;
 Image.SymTabSection:=nil;

 SymTabSectionIndex:=-1;

 for SectionIndex:=0 to Image.Sections.Count-1 do begin
  Section:=Image.Sections[SectionIndex];
  if Section.sh_name>0 then begin
   if Image.SHStrTabSection.Stream.Seek(Section.sh_name,soBeginning)<>Section.sh_name then begin
    TPACCInstance(Instance).AddError('Stream seek error',nil,true);
   end;
   Name:='';
   while Image.SHStrTabSection.Stream.Position<Image.SHStrTabSection.Stream.Size do begin
    Image.SHStrTabSection.Stream.ReadBuffer(c,SizeOf(AnsiChar));
    if c=#0 then begin
     break;
    end else begin
     Name:=Name+c;
    end;
   end;
   Section.Name:=Name;
   if Name='.strtab' then begin
    Image.StrTabSection:=Section;
   end else if Name='.symtab' then begin
    Image.SymTabSection:=Section;
    SymTabSectionIndex:=SectionIndex;
   end;
  end;
 end;

 for SectionIndex:=0 to Image.Sections.Count-1 do begin
  Section:=Image.Sections[SectionIndex];
  if Section.sh_link>0 then begin
   Section.LinkSection:=Image.Sections[Section.sh_link];
  end else begin
   Section.LinkSection:=nil;
  end;
  if (Section.sh_type in [SHT_REL,SHT_RELA]) and (Section.sh_info>0) then begin
   Section.InfoSection:=Image.Sections[Section.sh_Info];
  end else begin
   Section.InfoSection:=nil;
  end;
 end;

 if Image.SHStrTabSection.Name<>'.shstrtab' then begin
  TPACCInstance(Instance).AddError('".shstrtab" section have wrong name',nil,true);
 end;

 if not assigned(Image.StrTabSection) then begin
  TPACCInstance(Instance).AddError('No ".strtab" section',nil,true);
 end;

 if not assigned(Image.SymTabSection) then begin
  TPACCInstance(Instance).AddError('No ".symtab" section',nil,true);
 end;

 Image.SymTabSection.Stream.Seek(0,soBeginning);
 while Image.SymTabSection.Stream.Position<Image.SymTabSection.Stream.Size do begin
  Symbol:=TPACCLinker_ELF_ELF_Symbol.Create(self);
  Symbol.Index_:=Image.Symbols.Add(Symbol);
  if fIs64Bit then begin
   Image.SymTabSection.Stream.ReadBuffer(ELF3264Sym.ELF64Sym,SizeOf(TELF64Sym));
   Symbol.st_name:=ELF3264Sym.ELF64Sym.st_name;
   Symbol.st_info:=ELF3264Sym.ELF64Sym.st_info;
   Symbol.st_other:=ELF3264Sym.ELF64Sym.st_other;
   Symbol.st_shndx:=ELF3264Sym.ELF64Sym.st_shndx;
   Symbol.st_value:=ELF3264Sym.ELF64Sym.st_value;
   Symbol.st_size:=ELF3264Sym.ELF64Sym.st_size;
  end else begin
   Image.SymTabSection.Stream.ReadBuffer(ELF3264Sym.ELF32Sym,SizeOf(TELF32Sym));
   Symbol.st_name:=ELF3264Sym.ELF32Sym.st_name;
   Symbol.st_info:=ELF3264Sym.ELF32Sym.st_info;
   Symbol.st_other:=ELF3264Sym.ELF32Sym.st_other;
   Symbol.st_shndx:=ELF3264Sym.ELF32Sym.st_shndx;
   Symbol.st_value:=ELF3264Sym.ELF32Sym.st_value;
   Symbol.st_size:=ELF3264Sym.ELF32Sym.st_size;
  end;
  if Symbol.st_name>0 then begin
   if Image.StrTabSection.Stream.Seek(Symbol.st_name,soBeginning)<>Symbol.st_name then begin
    TPACCInstance(Instance).AddError('Stream seek error',nil,true);
   end;
   Name:='';
   while Image.StrTabSection.Stream.Position<Image.StrTabSection.Stream.Size do begin
    Image.StrTabSection.Stream.ReadBuffer(c,SizeOf(AnsiChar));
    if c=#0 then begin
     break;
    end else begin
     Name:=Name+c;
    end;
   end;
   Symbol.Name:=Name;
  end;
  if Symbol.st_shndx=SHN_UNDEF then begin
   Symbol.Section:=nil;
  end else if Symbol.st_shndx<Image.Sections.Count then begin
   Symbol.Section:=Image.Sections[Symbol.st_shndx];
   Symbol.Section.Symbols.Add(Symbol);
  end else if Symbol.st_shndx<SHN_LORESERVE then begin
   TPACCInstance(Instance).AddError('Symbol section index out of range',nil,true);
  end else begin
   Symbol.Section:=nil;
  end;
 end;

 for SectionIndex:=0 to Image.Sections.Count-1 do begin
  Section:=Image.Sections[SectionIndex];
  case Section.sh_type of
   SHT_REL,SHT_RELA:begin
    if Section.sh_info<Image.Sections.Count then begin
     TargetSection:=Image.Sections[Section.sh_info];
     if (((Section.sh_type=SHT_REL) and (Section.Name=('.rel'+TargetSection.Name))) or
         ((Section.sh_type=SHT_RELA) and (Section.Name=('.rela'+TargetSection.Name)))) and
        (Section.sh_link=SymTabSectionIndex) then begin
      Section.Stream.Seek(0,soBeginning);
      while Section.Stream.Position<Section.Stream.Size do begin
       Relocation:=TPACCLinker_ELF_ELF_Relocation.Create(self);
       TargetSection.Relocations.Add(Relocation);
       Relocation.Section:=TargetSection;
       case Section.sh_type of
        SHT_REL:begin
         if Is64Bit then begin
          Section.Stream.ReadBuffer(ELF3264Rel.ELF64Rel,SizeOf(TELF64Rel));
          Relocation.r_offset:=ELF3264Rel.ELF64Rel.r_offset;
          Relocation.r_info:=ELF3264Rel.ELF64Rel.r_info;
          Relocation.r_addend:=0;
         end else begin
          Section.Stream.ReadBuffer(ELF3264Rel.ELF32Rel,SizeOf(TELF32Rel));
          Relocation.r_offset:=ELF3264Rel.ELF32Rel.r_offset;
          Relocation.r_info:=((TELFXWord(ELF3264Rel.ELF32Rel.r_info) and $ffffff00) shl 24) or (ELF3264Rel.ELF32Rel.r_info and $ff);
          Relocation.r_addend:=0;
         end;
        end;
        else {SHT_RELA:}begin
         if Is64Bit then begin
          Section.Stream.ReadBuffer(ELF3264Rela.ELF64Rela,SizeOf(TELF64Rela));
          Relocation.r_offset:=ELF3264Rela.ELF64Rela.r_offset;
          Relocation.r_info:=ELF3264Rela.ELF64Rela.r_info;
          Relocation.r_addend:=ELF3264Rela.ELF64Rela.r_addend;
         end else begin
          Section.Stream.ReadBuffer(ELF3264Rela.ELF32Rela,SizeOf(TELF32Rela));
          Relocation.r_offset:=ELF3264Rela.ELF32Rela.r_offset;
          Relocation.r_info:=((TELFXWord(ELF3264Rela.ELF32Rela.r_info) and $ffffff00) shl 24) or (ELF3264Rela.ELF32Rela.r_info and $ff);
          Relocation.r_addend:=ELF3264Rela.ELF32Rela.r_addend;
         end;
        end;
       end;
       SymbolIndex:=Relocation.r_info shr 32;
       if SymbolIndex<Image.Symbols.Count then begin
        Relocation.Symbol:=Image.Symbols[SymbolIndex];
       end else begin
        TPACCInstance(Instance).AddError('Section "'+Section.Name+'" relocation symbol index out of range',nil,true);
       end;
      end;
     end else begin
      TPACCInstance(Instance).AddError('Corrupt relocation section "'+Section.Name+'"',nil,true);
     end;
    end else begin
     TPACCInstance(Instance).AddError('Section "'+Section.Name+'" section index out of range',nil,true);
    end;
   end;
  end;
 end;

end;

procedure TPACCLinker_ELF_ELF.AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker_ELF_ELF.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
type PPACCUInt8s=^TPACCUInt8s;
     TPACCUInt8s=array[0..65535] of TPACCUInt8;
 procedure WriteNullPadding(const Stream:TStream;Value:TPACCInt64);
 var PartCount:TPACCInt64;
 begin
  while Value>0 do begin
   if Value<SizeOf(NullBytes) then begin
    PartCount:=Value;
   end else begin
    PartCount:=SizeOf(NullBytes);
   end;
   Stream.WriteBuffer(NullBytes[0],PartCount);
   dec(Value,PartCount);
  end;
 end;
 procedure WriteNOPPadding(const Stream:TStream;Value:TPACCInt64);
  procedure WriteByte(const b:TPACCUInt8);
  begin
   Stream.WriteBuffer(b,SizeOf(TPACCUInt8));
  end;
  procedure WriteByteCount(const b:TPACCUInt8;c:TPACCInt64);
  begin
   while c>0 do begin
    Stream.WriteBuffer(b,SizeOf(TPACCUInt8));
    dec(c);
   end;
  end;
 var PartCount:TPACCInt64;
 begin
  case fMachine of
   EM_386:begin
    while Value>0 do begin
     if Value<16 then begin
      PartCount:=Value;
     end else begin
      PartCount:=15;
     end;
     case PartCount of
      1:begin
       // nop
       WriteByte($90);
       dec(Value);
      end;
      2:begin
       // xchg ax, ax (o16 nop)
       WriteByte($66);
       WriteByte($90);
       dec(Value,2);
      end;
      3:begin
       // lea esi,[esi+byte 0]
       WriteByte($8d);
       WriteByte($76);
       WriteByte($00);
       dec(Value,3);
      end;
      4:begin
       // lea esi,[esi*1+byte 0]
       WriteByte($8d);
       WriteByte($74);
       WriteByte($26);
       WriteByte($00);
       dec(Value,4);
      end;
      5:begin
       // nop
       WriteByte($90);
       // lea esi,[esi*1+byte 0]
       WriteByte($8d);
       WriteByte($74);
       WriteByte($26);
       WriteByte($00);
       dec(Value,5);
      end;
      6:begin
       // lea esi,[esi+dword 0]
       WriteByte($8d);
       WriteByte($b6);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,6);
      end;
      7:begin
       // lea esi,[esi*1+dword 0]
       WriteByte($8d);
       WriteByte($b4);
       WriteByte($26);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,7);
      end;
      8:begin
       // nop
       WriteByte($90);
       // lea esi,[esi*1+dword 0]
       WriteByte($8d);
       WriteByte($b4);
       WriteByte($26);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,8);
      end;
      9..15:begin
       // jmp $+9; nop fill .. jmp $+15; nop fill ..
       WriteByte($eb);
       WriteByte(PartCount-2);
       WriteByteCount($90,PartCount-2);
       dec(Value,PartCount);
      end;
     end;
    end;
   end;
   EM_X86_64:begin
    while Value>0 do begin
     if Value<16 then begin
      PartCount:=Value;
     end else begin
      PartCount:=15;
     end;
     case PartCount of
      1:begin
       // nop
       WriteByte($90);
       dec(Value);
      end;
      2:begin
       // xchg ax, ax (o16 nop)
       WriteByte($66);
       WriteByte($90);
       dec(Value,2);
      end;
      3:begin
       // nop(3)
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($00);
       dec(Value,3);
      end;
      4:begin
       // nop(4)
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($40);
       WriteByte($00);
       dec(Value,4);
      end;
      5:begin
       // nop(5)
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($44);
       WriteByte($00);
       WriteByte($00);
       dec(Value,5);
      end;
      6:begin
       // nop(6)
       WriteByte($66);
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($44);
       WriteByte($00);
       WriteByte($00);
       dec(Value,6);
      end;
      7:begin
       // nop(7)
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($80);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,7);
      end;
      8:begin
       // nop(8)
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($84);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,8);
      end;
      9:begin
       // nop(9)
       WriteByte($66);
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($84);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,9);
      end;
      10..15:begin
       // repeated-o16 cs: nop(10..15)
       WriteByteCount($66,PartCount-9);
       WriteByte($2e);
       WriteByte($0f);
       WriteByte($1f);
       WriteByte($84);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       WriteByte($00);
       dec(Value,PartCount);
      end;
     end;
    end;
   end;
   else begin
    while Value>0 do begin
     if Value<SizeOf(NullBytes) then begin
      PartCount:=Value;
     end else begin
      PartCount:=SizeOf(NullBytes);
     end;
     Stream.WriteBuffer(NullBytes[0],PartCount);
     dec(Value,PartCount);
    end;
   end;
  end;
 end;
var OutputImage:TPACCLinker_ELF_ELF_Image;
    ImageIndex,ImageSectionIndex,OutputImageSectionIndex,
    SymbolOffsetIndex,ImageSymbolIndex:TPACCInt32;
    Image:TPACCLinker_ELF_ELF_Image;
    ImageSection,ImageSectionSTab,ImageSectionSTabStr,
    OutputImageSection,CurrentOutputImageSection:TPACCLinker_ELF_ELF_Section;
    HasGNULinkOnce:boolean;
    ImageSymbol:TPACCLinker_ELF_ELF_Symbol;
    STabSym:PSTabSym;
    Position:TPACCInt64;
    OutputImageSymbolNameHashMap:TPACCRawByteStringHashMap;
 function FindSymbol(const SymbolName:TPACCRawByteString):TPACCLinker_ELF_ELF_Symbol;
 begin
  result:=OutputImageSymbolNameHashMap[SymbolName];
 end;
 function PutSymbol(const DestSection:TPACCLinker_ELF_ELF_Section;
                    const SymbolName:TPACCRawByteString;
                    st_value:TELF32Addr;
                    st_size:TELFWord;
                    st_info:TPACCUInt8;
                    st_other:TPACCUInt8;
                    st_shndx:TELFHalf):TPACCLinker_ELF_ELF_Symbol;
 begin
  result:=TPACCLinker_ELF_ELF_Symbol.Create(self);
  result.Index_:=OutputImage.Symbols.Add(result);
  result.Name:=SymbolName;
  result.st_info:=st_info;
  result.st_other:=st_other;
  result.st_shndx:=st_shndx;
  result.st_value:=st_value;
  result.st_size:=st_size;
  OutputImageSymbolNameHashMap[SymbolName]:=result;
  if result.st_shndx=SHN_UNDEF then begin
   result.Section:=nil;
  end else if result.st_shndx<OutputImage.Sections.Count then begin
   result.Section:=OutputImage.Sections[result.st_shndx];
   result.Section.Symbols.Add(result);
  end else if result.st_shndx<SHN_LORESERVE then begin
   TPACCInstance(Instance).AddError('Output symbol "'+SymbolName+'" section index out of range',nil,true);
  end else begin
   result.Section:=nil;
  end;
 end;
 function AddSymbol(const DestSection:TPACCLinker_ELF_ELF_Section;
                    const SymbolName:TPACCRawByteString;
                    st_value:TELF32Addr;
                    st_size:TELFWord;
                    st_info:TPACCUInt8;
                    st_other:TPACCUInt8;
                    st_shndx:TELFHalf):TPACCLinker_ELF_ELF_Symbol;
 var SymBind,SymType,SymVis,OtherSymBind,OtherSymType,OtherSymVis,NewVis:TPACCUInt32;
     DoPatch:boolean;
 begin
  SymBind:=ELF_ST_BIND(st_info);
  SymType:=ELF_ST_TYPE(st_info);
  SymVis:=ELF_ST_VISIBILITY(st_other);
  result:=FindSymbol(SymbolName);
  if assigned(result) and (SymBind<>STB_LOCAL) then begin
   if result.st_shndx<>SHN_UNDEF then begin
    OtherSymBind:=ELF_ST_BIND(result.st_info);
    OtherSymType:=ELF_ST_TYPE(result.st_info);
    OtherSymVis:=ELF_ST_VISIBILITY(result.st_other);
    if OtherSymVis=STV_DEFAULT then begin
     NewVis:=SymVis;
    end else if SymVis=STV_DEFAULT then begin
     NewVis:=OtherSymVis;
    end else begin
     NewVis:=Min(SymVis,OtherSymVis);
    end;
    result.st_other:=(result.st_other and not ELF_ST_VISIBILITY($ffffffff)) or NewVis;
    st_other:=result.st_other;
    DoPatch:=false;
    if st_shndx=SHN_UNDEF then begin
     // Ignore
    end else if (SymBind=STB_GLOBAL) and (OtherSymBind=STB_WEAK) then begin
     // Global overrides weak
     DoPatch:=true;
    end else if (SymBind=STB_WEAK) and (OtherSymBind=STB_GLOBAL) then begin
     // Weak is ignored if it is already global
    end else if (SymBind=STB_WEAK) and (OtherSymBind=STB_WEAK) then begin
     // Weak is ignored if it is weak
    end else if (SymVis=STV_HIDDEN) and (OtherSymVis=STV_INTERNAL) then begin
     // Internal > Hidden
    end else if (result.st_shndx=SHN_COMMON) and ((st_shndx<SHN_LORESERVE) or (st_shndx=SHN_COMMON)) then begin
     DoPatch:=true;
    end else begin
     TPACCInstance(Instance).AddError('Duplicate defined symbol name "'+SymbolName+'"',nil,false);
    end;
   end else begin
    DoPatch:=true;
   end;
   if DoPatch then begin
    result.st_info:=ELF_ST_INFO(SymBind,SymType);
    result.st_other:=st_other;
    result.st_shndx:=st_shndx;
    result.st_value:=st_value;
    result.st_size:=st_size;
   end;
  end else begin
   result:=PutSymbol(DestSection,SymbolName,st_value,st_size,st_info,st_other,st_shndx);
  end;
 end;
begin

 OutputImageSymbolNameHashMap:=TPACCRawByteStringHashMap.Create;
 try

  OutputImage:=TPACCLinker_ELF_ELF_Image.Create(self);
  try

   OutputImage.Name:=AOutputFileName;

   begin
    OutputImageSection:=TPACCLinker_ELF_ELF_Section.Create(self);
    OutputImageSection.Index_:=OutputImage.Sections.Add(OutputImageSection);
    OutputImageSection.Name:='';
    OutputImageSection.sh_type:=0;
    OutputImageSection.sh_flags:=0;
    OutputImageSection.sh_addralign:=0;
    OutputImageSection.sh_entsize:=0;
    OutputImageSection.sh_link:=0;
    OutputImageSection.sh_info:=0;
   end;

   HasGNULinkOnce:=false;

   for ImageIndex:=0 to Images.Count-1 do begin

    Image:=Images[ImageIndex];

    SymbolOffsetIndex:=OutputImage.Symbols.Count;

    ImageSectionSTab:=nil;
    ImageSectionSTabStr:=nil;

    for ImageSectionIndex:=0 to Image.Sections.Count-1 do begin

     ImageSection:=Image.Sections[ImageSectionIndex];

     ImageSection.Merged:=false;

     ImageSection.LinkOnce:=HasGNULinkOnce and (ImageSection.Name='.gnu.linkonce');

     if (ImageSection.sh_type in [SHT_PROGBITS,
                                  SHT_REL,
                                  SHT_RELA,
                                  SHT_NOBITS,
                                  SHT_PREINIT_ARRAY,
                                  SHT_INIT_ARRAY,
                                  SHT_FINI_ARRAY]) and
        (ImageSection.Name<>'.stabstr') and
        ((not HasGNULinkOnce) or (HasGNULinkOnce and (ImageSection.Name<>'.gnu.linkonce'))) then begin

      if (not HasGNULinkOnce) and (ImageSection.Name='.gnu.linkonce') then begin
       HasGNULinkOnce:=true;
      end;

      OutputImageSection:=nil;
      for OutputImageSectionIndex:=0 to OutputImage.Sections.Count-1 do begin
       CurrentOutputImageSection:=OutputImage.Sections[OutputImageSectionIndex];
       if CurrentOutputImageSection.Name=ImageSection.Name then begin
        OutputImageSection:=CurrentOutputImageSection;
        break;
       end;
      end;

      if assigned(OutputImageSection) then begin
       OutputImageSection.sh_addralign:=Max(OutputImageSection.sh_addralign,ImageSection.sh_addralign);
      end else begin
       OutputImageSection:=TPACCLinker_ELF_ELF_Section.Create(self);
       OutputImageSection.Index_:=OutputImage.Sections.Add(OutputImageSection);
       OutputImageSection.Name:=ImageSection.Name;
       OutputImageSection.sh_type:=ImageSection.sh_type;
       OutputImageSection.sh_flags:=ImageSection.sh_flags;
       OutputImageSection.sh_addralign:=Max(1,ImageSection.sh_addralign);
       OutputImageSection.sh_entsize:=ImageSection.sh_entsize;
       OutputImageSection.sh_link:=0;
       OutputImageSection.sh_info:=0;
      end;

      ImageSection.Stream.Seek(0,soBeginning);
      OutputImageSection.Stream.Seek(OutputImageSection.Stream.Size,soBeginning);
      if ImageSection.Name='.stab' then begin
       ImageSectionSTab:=ImageSection;
      end else if ImageSection.Name='.stabstr' then begin
       ImageSectionSTabStr:=ImageSection;
      end else begin
       WriteNullPadding(OutputImageSection.Stream,Max(0,TPACCInt64((OutputImageSection.Stream.Size+TPACCInt64(ImageSection.sh_addralign-1)) and not TPACCInt64(ImageSection.sh_addralign-1))-OutputImageSection.Stream.Size));
      end;
      ImageSection.MergedOffset:=OutputImageSection.Stream.Size;
      ImageSection.MergedToSection:=OutputImageSection;
      if ImageSection.sh_type=SHT_NOBITS then begin
       WriteNullPadding(OutputImageSection.Stream,ImageSection.Stream.Size);
      end else begin
       OutputImageSection.Stream.CopyFrom(ImageSection.Stream,ImageSection.Stream.Size);
      end;

      ImageSection.Merged:=true;

     end;

    end;

    if assigned(ImageSectionSTab) and assigned(ImageSectionSTabStr) and
       ImageSectionSTab.Merged and ImageSectionSTabStr.Merged then begin
     Position:=0;
     while (Position+SizeOf(TSTabSym))<=ImageSectionSTab.Stream.Size do begin
      STabSym:=pointer(@PPACCUInt8s(ImageSectionSTab.MergedToSection.Stream.Memory)^[ImageSectionSTab.MergedOffset+Position]);
      inc(STabSym^.n_strx,ImageSectionSTabStr.MergedOffset);
      inc(Position,SizeOf(TSTabSym));
     end;
    end;

    for ImageSectionIndex:=0 to Image.Sections.Count-1 do begin
     ImageSection:=Image.Sections[ImageSectionIndex];
     if ImageSection.Merged and (ImageSection.MergedOffset=0) then begin
      if assigned(ImageSection.LinkSection) then begin
       OutputImageSection:=ImageSection.MergedToSection;
       OutputImageSection.LinkSection:=ImageSection.LinkSection.MergedToSection;
       if assigned(OutputImageSection.LinkSection) then begin
        OutputImageSection.sh_link:=OutputImageSection.LinkSection.Index_;
       end;
      end;
      case ImageSection.sh_type of
       SHT_REL,SHT_RELA:begin
        if assigned(ImageSection.InfoSection) and assigned(ImageSection.InfoSection.MergedToSection) then begin
         OutputImageSection:=ImageSection.MergedToSection;
         OutputImageSection.InfoSection:=ImageSection.InfoSection.MergedToSection;
         OutputImageSection.sh_info:=OutputImageSection.InfoSection.Index_;
        end;
       end;
      end;
     end;
    end;

    for ImageSymbolIndex:=0 to Image.Symbols.Count-1 do begin
     ImageSymbol:=Image.Symbols[ImageSymbolIndex];
     if (ImageSymbol.st_shndx=SHN_UNDEF) or (ImageSymbol.st_shndx>=SHN_LORESERVE) then begin
      ImageSymbol.MergedSymbol:=AddSymbol(OutputImage.SymTabSection,
                                          ImageSymbol.Name,
                                          ImageSymbol.st_value,
                                          ImageSymbol.st_size,
                                          ImageSymbol.st_info,
                                          ImageSymbol.st_other,
                                          ImageSymbol.st_shndx);
     end else begin
      ImageSection:=ImageSymbol.Section;
      if assigned(ImageSection) then begin
       if ImageSection.LinkOnce then begin
        if ELF_ST_BIND(ImageSymbol.st_info)<>STB_LOCAL then begin
         ImageSymbol.MergedSymbol:=FindSymbol(ImageSymbol.Name);
        end;
        continue;
       end;
       OutputImageSection:=ImageSection.MergedToSection;
       if assigned(OutputImageSection) then begin
        ImageSymbol.MergedSymbol:=AddSymbol(OutputImage.SymTabSection,
                                            ImageSymbol.Name,
                                            ImageSymbol.st_value+OutputImageSection.MergedOffset,
                                            ImageSymbol.st_size,
                                            ImageSymbol.st_info,
                                            ImageSymbol.st_other,
                                            OutputImageSection.Index_);
       end;
      end;
     end;
    end;

   end;

  finally
   OutputImage.Free;
  end;

 finally
  OutputImageSymbolNameHashMap.Free;
 end;

end;

end.
