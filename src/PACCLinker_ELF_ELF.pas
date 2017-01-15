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

type // File header
     PELF32EHdr=^TELF32EHdr;
     TELF32EHdr=packed record
      e_ident:array[0..EI_NIDENT-1] of AnsiChar;
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
      e_ident:array[0..EI_NIDENT-1] of AnsiChar;
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

type TPACCLinker_ELF_ELF=class;

     TPACCLinker_ELF_ELF=class(TPACCLinker)
      private
      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       procedure AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String); override;

       procedure AddExport(const ASymbolName,AExportName:TPUCUUTF8String); override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); override;

       procedure AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); override;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

      published

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
function ELF74_R_INFO(const s,t:TELFXWord):TELFXWord;

implementation

uses PACCInstance;

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

function ELF74_R_INFO(const s,t:TELFXWord):TELFXWord;
begin
 result:=(s shl 32) or (t and $ffffffff);
end;

constructor TPACCLinker_ELF_ELF.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);
end;

destructor TPACCLinker_ELF_ELF.Destroy;
begin


 inherited Destroy;
end;

procedure TPACCLinker_ELF_ELF.AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker_ELF_ELF.AddExport(const ASymbolName,AExportName:TPUCUUTF8String);
begin
end;

procedure TPACCLinker_ELF_ELF.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
begin
end;

procedure TPACCLinker_ELF_ELF.AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); 
begin
end;

procedure TPACCLinker_ELF_ELF.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;

end.
