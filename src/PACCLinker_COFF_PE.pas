unit PACCLinker_COFF_PE;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap,PACCLinker;

type TPACCLinker_COFF_PE=class;

     PPACCLinker_COFF_PERelocation=^TPACCLinker_COFF_PERelocation;
     TPACCLinker_COFF_PERelocation=record
      VirtualAddress:TPACCUInt32;
      Symbol:TPACCUInt32;
      RelocationType:TPACCUInt16;
     end;

     TPACCLinker_COFF_PERelocations=array of TPACCLinker_COFF_PERelocation;

     TPACCLinker_COFF_PESection=class
      private
       fLinker:TPACCLinker_COFF_PE;
       fName:TPACCRawByteString;
       fStream:TMemoryStream;
       fAlignment:TPACCInt32;
       fVirtualAddress:TPACCUInt64;
       fCharacteristics:TPACCUInt32;
      protected
       Relocations:TPACCLinker_COFF_PERelocations;
      public
       constructor Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const AVirtualAddress:TPACCUInt64;const ACharacteristics:TPACCUInt32); reintroduce;
       destructor Destroy; override;
      published
       property Linker:TPACCLinker_COFF_PE read fLinker;
       property Name:TPACCRawByteString read fName;
       property Stream:TMemoryStream read fStream;
       property Alignment:TPACCInt32 read fAlignment write fAlignment;
       property VirtualAddress:TPACCUInt64 read fVirtualAddress write fVirtualAddress;
       property Characteristics:TPACCUInt32 read fCharacteristics write fCharacteristics;
     end;

     TPACCLinker_COFF_PESectionList=class(TList)
      private
       function GetItem(const Index:TPACCInt):TPACCLinker_COFF_PESection;
       procedure SetItem(const Index:TPACCInt;Node:TPACCLinker_COFF_PESection);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const Index:TPACCInt]:TPACCLinker_COFF_PESection read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PESymbolList=class;

     TPACCLinker_COFF_PESymbol=class
      private
       fLinker:TPACCLinker_COFF_PE;
       fName:TPACCRawByteString;
       fSection:TPACCLinker_COFF_PESection;
       fValue:TPACCInt64;
       fType:TPACCInt32;
       fClass:TPACCInt32;
       fSubSymbols:TPACCLinker_COFF_PESymbolList;
      public
       constructor Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const ASection:TPACCLinker_COFF_PESection;const AValue:TPACCInt64;const AType,AClass:TPACCInt32); reintroduce;
       destructor Destroy; override;
      published
       property Linker:TPACCLinker_COFF_PE read fLinker;
       property Name:TPACCRawByteString read fName;
       property Section:TPACCLinker_COFF_PESection read fSection;
       property Value:TPACCInt64 read fValue;
       property Type_:TPACCInt32 read fType;
       property Class_:TPACCInt32 read fClass;
       property SubSymbols:TPACCLinker_COFF_PESymbolList read fSubSymbols;
     end;

     TPACCLinker_COFF_PESymbolList=class(TList)
      private
       function GetItem(const Index:TPACCInt):TPACCLinker_COFF_PESymbol;
       procedure SetItem(const Index:TPACCInt;Node:TPACCLinker_COFF_PESymbol);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const Index:TPACCInt]:TPACCLinker_COFF_PESymbol read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PE=class(TPACCLinker)
      private

       fMachine:TPACCUInt16;

       fSections:TPACCLinker_COFF_PESectionList;

       fSymbols:TPACCLinker_COFF_PESymbolList;

      public

       constructor Create(const AInstance:TObject); reintroduce;
       destructor Destroy; override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); override;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

      published

       property Machine:TPACCUInt16 read fMachine;
       
       property Sections:TPACCLinker_COFF_PESectionList read fSections;

       property Symbols:TPACCLinker_COFF_PESymbolList read fSymbols;

     end;

implementation

uses PACCInstance,PACCTarget_x86_32{,PACCTarget_x86_64_Win64};

const MZEXEHeaderSize=128;
      MZEXEHeaderBytes:array[0..MZEXEHeaderSize-1] of TPACCUInt8=
       ($4d,$5a,$80,$00,$01,$00,$00,$00,$04,$00,$10,$00,$ff,$ff,$00,$00,
        $40,$01,$00,$00,$00,$00,$00,$00,$40,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,
        $00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$00,$80,$00,$00,$00,
        $0e,$1f,$ba,$0e,$00,$b4,$09,$cd,$21,$b8,$01,$4c,$cd,$21,$54,$68,
        $69,$73,$20,$70,$72,$6f,$67,$72,$61,$6d,$20,$63,$61,$6e,$6e,$6f,
        $74,$20,$62,$65,$20,$72,$75,$6e,$20,$69,$6e,$20,$44,$4f,$53,$20,
        $6d,$6f,$64,$65,$2e,$0d,$0a,$24,$00,$00,$00,$00,$00,$00,$00,$00);

      IMPORTED_NAME_OFFSET=$00000002;
      IMAGE_ORDINAL_FLAG32=$80000000;
      IMAGE_ORDINAL_MASK32=$0000ffff;
      IMAGE_ORDINAL_FLAG64=TPACCUInt64($8000000000000000);
      IMAGE_ORDINAL_MASK64=TPACCUInt64($0000ffff);

      RTL_CRITSECT_TYPE=0;
      RTL_RESOURCE_TYPE=1;

      DLL_PROCESS_ATTACH=1;
      DLL_THREAD_ATTACH=2;
      DLL_THREAD_DETACH=3;
      DLL_PROCESS_DETACH=0;

      IMAGE_SizeHeader=20;

      IMAGE_FILE_RELOCS_STRIPPED=$0001;
      IMAGE_FILE_EXECUTABLE_IMAGE=$0002;
      IMAGE_FILE_LINE_NUMS_STRIPPED=$0004;
      IMAGE_FILE_LOCAL_SYMS_STRIPPED=$0008;
      IMAGE_FILE_AGGRESIVE_WS_TRIM=$0010;
      IMAGE_FILE_BYTES_REVERSED_LO=$0080;
      IMAGE_FILE_32BIT_MACHINE=$0100;
      IMAGE_FILE_DEBUG_STRIPPED=$0200;
      IMAGE_FILE_REMOVABLE_RUN_FROM_SWAP=$0400;
      IMAGE_FILE_NET_RUN_FROM_SWAP=$0800;
      IMAGE_FILE_SYSTEM=$1000;
      IMAGE_FILE_DLL=$2000;
      IMAGE_FILE_UP_SYSTEM_ONLY=$4000;
      IMAGE_FILE_BYTES_REVERSED_HI=$8000;

      IMAGE_FILE_MACHINE_UNKNOWN=0;
      IMAGE_FILE_MACHINE_I386=$14c;
      IMAGE_FILE_MACHINE_R3000=$162;
      IMAGE_FILE_MACHINE_R4000=$166;
      IMAGE_FILE_MACHINE_R10000=$168;
      IMAGE_FILE_MACHINE_ALPHA=$184;
      IMAGE_FILE_MACHINE_POWERPC=$1f0;
      IMAGE_FILE_MACHINE_AMD64=$8664;

      IMAGE_DLLCHARACTERISTICS_DYNAMIC_BASE=$0040;
      IMAGE_DLLCHARACTERISTICS_FORCE_INTEGRITY=$0080;
      IMAGE_DLLCHARACTERISTICS_NX_COMPAT=$0100;
      IMAGE_DLLCHARACTERISTICS_NO_ISOLATION=$0200;
      IMAGE_DLLCHARACTERISTICS_NO_SEH=$0400;
      IMAGE_DLLCHARACTERISTICS_NO_BIND=$0800;
      IMAGE_DLLCHARACTERISTICS_WDM_DRIVER=$2000;
      IMAGE_DLLCHARACTERISTICS_TERMINAL_SERVER_AWARE=$8000;

      IMAGE_NUMBEROF_DIRECTORY_ENTRIES=16;

      IMAGE_SUBSYSTEM_UNKNOWN=0;
      IMAGE_SUBSYSTEM_NATIVE=1;
      IMAGE_SUBSYSTEM_WINDOWS_GUI=2;
      IMAGE_SUBSYSTEM_WINDOWS_CUI=3;
      IMAGE_SUBSYSTEM_OS2_CUI=5;
      IMAGE_SUBSYSTEM_POSIX_CUI=7;
      IMAGE_SUBSYSTEM_WINDOWS_CE_GUI=9;
      IMAGE_SUBSYSTEM_EFI_APPLICATION=10;
      IMAGE_SUBSYSTEM_EFI_BOOT_SERVICE_DRIVER=11;
      IMAGE_SUBSYSTEM_EFI_RUNTIME_DRIVER=12;
      IMAGE_SUBSYSTEM_EFI_ROM=13;
      IMAGE_SUBSYSTEM_XBOX=14;
      IMAGE_SUBSYSTEM_WINDOWS_BOOT_APPLICATION=16;

      IMAGE_DIRECTORY_ENTRY_EXPORT=0;
      IMAGE_DIRECTORY_ENTRY_IMPORT=1;
      IMAGE_DIRECTORY_ENTRY_RESOURCE=2;
      IMAGE_DIRECTORY_ENTRY_EXCEPTION=3;
      IMAGE_DIRECTORY_ENTRY_SECURITY=4;
      IMAGE_DIRECTORY_ENTRY_BASERELOC=5;
      IMAGE_DIRECTORY_ENTRY_DEBUG=6;
      IMAGE_DIRECTORY_ENTRY_COPYRIGHT=7;
      IMAGE_DIRECTORY_ENTRY_GLOBALPTR=8;
      IMAGE_DIRECTORY_ENTRY_TLS=9;
      IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG=10;
      IMAGE_DIRECTORY_ENTRY_BOUND_IMPORT=11;
      IMAGE_DIRECTORY_ENTRY_IAT=12;

      IMAGE_SIZEOF_SHORT_NAME=8;
      
      IMAGE_SCN_TYIMAGE_REG=$00000000;
      IMAGE_SCN_TYIMAGE_DSECT=$00000001;
      IMAGE_SCN_TYIMAGE_NOLOAD=$00000002;
      IMAGE_SCN_TYIMAGE_GROUP=$00000004;
      IMAGE_SCN_TYIMAGE_NO_PAD=$00000008;
      IMAGE_SCN_TYIMAGE_COPY=$00000010;
      IMAGE_SCN_CNT_CODE=$00000020;
      IMAGE_SCN_CNT_INITIALIZED_DATA=$00000040;
      IMAGE_SCN_CNT_UNINITIALIZED_DATA=$00000080;
      IMAGE_SCN_LNK_OTHER=$00000100;
      IMAGE_SCN_LNK_INFO=$00000200;
      IMAGE_SCN_TYIMAGE_OVER=$0000400;
      IMAGE_SCN_LNK_REMOVE=$00000800;
      IMAGE_SCN_LNK_COMDAT=$00001000;
      IMAGE_SCN_MEM_PROTECTED=$00004000;
      IMAGE_SCN_MEM_FARDATA=$00008000;
      IMAGE_SCN_MEM_SYSHEAP=$00010000;
      IMAGE_SCN_MEM_PURGEABLE=$00020000;
      IMAGE_SCN_MEM_16BIT=$00020000;
      IMAGE_SCN_MEM_LOCKED=$00040000;
      IMAGE_SCN_MEM_PRELOAD=$00080000;
      IMAGE_SCN_ALIGN_1BYTES=$00100000;
      IMAGE_SCN_ALIGN_2BYTES=$00200000;
      IMAGE_SCN_ALIGN_4BYTES=$00300000;
      IMAGE_SCN_ALIGN_8BYTES=$00400000;
      IMAGE_SCN_ALIGN_16BYTES=$00500000;
      IMAGE_SCN_ALIGN_32BYTES=$00600000;
      IMAGE_SCN_ALIGN_64BYTES=$00700000;
      IMAGE_SCN_ALIGN_1286BYTES=$00800000;
      IMAGE_SCN_ALIGN_256BYTES=$00900000;
      IMAGE_SCN_ALIGN_512BYTES=$00a00000;
      IMAGE_SCN_ALIGN_1024BYTES=$00b00000;
      IMAGE_SCN_ALIGN_2048BYTES=$00c00000;
      IMAGE_SCN_ALIGN_4096BYTES=$00d00000;
      IMAGE_SCN_ALIGN_8192BYTES=$00e00000;
      IMAGE_SCN_ALIGN_MASK=$00f00000;
      IMAGE_SCN_ALIGN_SHIFT=20;
      IMAGE_SCN_LNK_NRELOC_OVFL=$01000000;
      IMAGE_SCN_MEM_DISCARDABLE=$02000000;
      IMAGE_SCN_MEM_NOT_CACHED=$04000000;
      IMAGE_SCN_MEM_NOT_PAGED=$08000000;
      IMAGE_SCN_MEM_SHARED=$10000000;
      IMAGE_SCN_MEM_EXECUTE=$20000000;
      IMAGE_SCN_MEM_READ=$40000000;
      IMAGE_SCN_MEM_WRITE=TPACCUInt32($80000000);
      IMAGE_SCN_CNT_RESOURCE:int64=$100000000;

      IMAGE_SCN_MAX_RELOC=$ffff;

      IMAGE_REL_BASED_ABSOLUTE=0;
      IMAGE_REL_BASED_HIGH=1;
      IMAGE_REL_BASED_LOW=2;
      IMAGE_REL_BASED_HIGHLOW=3;
      IMAGE_REL_BASED_HIGHADJ=4;
      IMAGE_REL_BASED_MIPS_JMPADDR=5;
      IMAGE_REL_BASED_ARM_MOV32A=5;
      IMAGE_REL_BASED_SECTION=6;
      IMAGE_REL_BASED_REL32=7;
      IMAGE_REL_BASED_ARM_MOV32T=7;
      IMAGE_REL_BASED_MIPS_JMPADDR16=9;
      IMAGE_REL_BASED_IA64_IMM64=9;
      IMAGE_REL_BASED_DIR64=10;
      IMAGE_REL_BASED_HIGH3ADJ=11;

      IMAGE_REL_I386_ABSOLUTE=$0000;
      IMAGE_REL_I386_DIR16=$0001;
      IMAGE_REL_I386_REL16=$0002;
      IMAGE_REL_I386_DIR32=$0006;
      IMAGE_REL_I386_DIR32NB=$0007;
      IMAGE_REL_I386_SEG12=$0009;
      IMAGE_REL_I386_SECTION=$000a;
      IMAGE_REL_I386_SECREL=$000b;
      IMAGE_REL_I386_TOKEN=$000c;
      IMAGE_REL_I386_SECREL7=$000d;
      IMAGE_REL_I386_REL32=$0014;

      IMAGE_REL_AMD64_ABSOLUTE=$0000;
      IMAGE_REL_AMD64_ADDR64=$0001;
      IMAGE_REL_AMD64_ADDR32=$0002;
      IMAGE_REL_AMD64_ADDR32NB=$0003;
      IMAGE_REL_AMD64_REL32=$0004;
      IMAGE_REL_AMD64_REL32_1=$0005;
      IMAGE_REL_AMD64_REL32_2=$0006;
      IMAGE_REL_AMD64_REL32_3=$0007;
      IMAGE_REL_AMD64_REL32_4=$0008;
      IMAGE_REL_AMD64_REL32_5=$0009;
      IMAGE_REL_AMD64_SECTION=$000a;
      IMAGE_REL_AMD64_SECREL=$000b;
      IMAGE_REL_AMD64_SECREL7=$000c;
      IMAGE_REL_AMD64_TOKEN=$000d;
      IMAGE_REL_AMD64_SREL32=$000e;
      IMAGE_REL_AMD64_PAIR=$000f;
      IMAGE_REL_AMD64_SSPAN32=$0010;

      IMAGE_REL_PPC_ABSOLUTE=$0000;
      IMAGE_REL_PPC_ADDR64=$0001;
      IMAGE_REL_PPC_ADDR32=$0002;
      IMAGE_REL_PPC_ADDR24=$0003;
      IMAGE_REL_PPC_ADDR16=$0004;
      IMAGE_REL_PPC_ADDR14=$0005;
      IMAGE_REL_PPC_REL24=$0006;
      IMAGE_REL_PPC_REL14=$0007;
      IMAGE_REL_PPC_ADDR32NB=$000a;
      IMAGE_REL_PPC_SECREL=$000b;
      IMAGE_REL_PPC_SECTION=$000c;
      IMAGE_REL_PPC_SECREL1=$000f;
      IMAGE_REL_PPC_REFHI=$0010;
      IMAGE_REL_PPC_REFLO=$0011;
      IMAGE_REL_PPC_PAIR=$0012;
      IMAGE_REL_PPC_SECRELLO=$0013;
      IMAGE_REL_PPC_GPREL=$0015;
      IMAGE_REL_PPC_TOKEN=$0016;

      IMAGE_SYM_CLASS_END_OF_FUNCTION=TPACCUInt8(-1); ///< Physical end of function
      IMAGE_SYM_CLASS_NULL=0;                   ///< No symbol
      IMAGE_SYM_CLASS_AUTOMATIC=1;              ///< Stack variable
      IMAGE_SYM_CLASS_EXTERNAL=2;               ///< External symbol
      IMAGE_SYM_CLASS_STATIC=3;                 ///< Static
      IMAGE_SYM_CLASS_REGISTER=4;               ///< Register variable
      IMAGE_SYM_CLASS_EXTERNAL_DEF=5;           ///< External definition
      IMAGE_SYM_CLASS_LABEL=6;                  ///< Label
      IMAGE_SYM_CLASS_UNDEFINED_LABEL=7;        ///< Undefined label
      IMAGE_SYM_CLASS_MEMBER_OF_STRUCT=8;       ///< Member of structure
      IMAGE_SYM_CLASS_ARGUMENT=9;               ///< Function argument
      IMAGE_SYM_CLASS_STRUCT_TAG=10;            ///< Structure tag
      IMAGE_SYM_CLASS_MEMBER_OF_UNION=11;       ///< Member of union
      IMAGE_SYM_CLASS_UNION_TAG=12;             ///< Union tag
      IMAGE_SYM_CLASS_TYPE_DEFINITION=13;       ///< Type definition
      IMAGE_SYM_CLASS_UNDEFINED_STATIC=14;      ///< Undefined static
      IMAGE_SYM_CLASS_ENUM_TAG=15;              ///< Enumeration tag
      IMAGE_SYM_CLASS_MEMBER_OF_ENUM=16;        ///< Member of enumeration
      IMAGE_SYM_CLASS_REGISTER_PARAM=17;        ///< Register parameter
      IMAGE_SYM_CLASS_BIT_FIELD=18;             ///< Bit field
      /// ".bb" or ".eb" - beginning or end of block
      IMAGE_SYM_CLASS_BLOCK=100;
      /// ".bf" or ".ef" - beginning or end of function
      IMAGE_SYM_CLASS_FUNCTION=101;
      IMAGE_SYM_CLASS_END_OF_STRUCT=102;        ///< End of structure
      IMAGE_SYM_CLASS_FILE=103;                 ///< File name
      /// Line number, reformatted as symbol
      IMAGE_SYM_CLASS_SECTION=104;
      IMAGE_SYM_CLASS_WEAK_EXTERNAL=105;        ///< Duplicate tag
      /// External symbol in dmert public lib
      IMAGE_SYM_CLASS_CLR_TOKEN=107;

      PAGE_NOACCESS=1;
      PAGE_READONLY=2;
      PAGE_READWRITE=4;
      PAGE_WRITECOPY=8;
      PAGE_EXECUTE=$10;
      PAGE_EXECUTE_READ=$20;
      PAGE_EXECUTE_READWRITE=$40;
      PAGE_EXECUTE_WRITECOPY=$80;
      PAGE_GUARD=$100;
      PAGE_NOCACHE=$200;
      MEM_COMMIT=$1000;
      MEM_RESERVE=$2000;
      MEM_DECOMMIT=$4000;
      MEM_RELEASE=$8000;
      MEM_FREE=$10000;
      MEM_PRIVATE=$20000;
      MEM_MAPPED=$40000;
      MEM_RESET=$80000;
      MEM_TOP_DOWN=$100000;
      SEC_FILE=$800000;
      SEC_IMAGE=$1000000;
      SEC_RESERVE=$4000000;
      SEC_COMMIT=$8000000;
      SEC_NOCACHE=$10000000;
      MEM_IMAGE=SEC_IMAGE;

      PE_SCN_TYPE_REG=$00000000;
      PE_SCN_TYPE_DSECT=$00000001;
      PE_SCN_TYPE_NOLOAD=$00000002;
      PE_SCN_TYPE_GROUP=$00000004;
      PE_SCN_TYPE_NO_PAD=$00000008;
      PE_SCN_TYPE_COPY=$00000010;
      PE_SCN_CNT_CODE=$00000020;
      PE_SCN_CNT_INITIALIZED_DATA=$00000040;
      PE_SCN_CNT_UNINITIALIZED_DATA=$00000080;
      PE_SCN_LNK_OTHER=$00000100;
      PE_SCN_LNK_INFO=$00000200;
      PE_SCN_TYPE_OVER=$0000400;
      PE_SCN_LNK_REMOVE=$00000800;
      PE_SCN_LNK_COMDAT=$00001000;
      PE_SCN_MEM_PROTECTED=$00004000;
      PE_SCN_MEM_FARDATA=$00008000;
      PE_SCN_MEM_SYSHEAP=$00010000;
      PE_SCN_MEM_PURGEABLE=$00020000;
      PE_SCN_MEM_16BIT=$00020000;
      PE_SCN_MEM_LOCKED=$00040000;
      PE_SCN_MEM_PRELOAD=$00080000;
      PE_SCN_ALIGN_1BYTES=$00100000;
      PE_SCN_ALIGN_2BYTES=$00200000;
      PE_SCN_ALIGN_4BYTES=$00300000;
      PE_SCN_ALIGN_8BYTES=$00400000;
      PE_SCN_ALIGN_16BYTES=$00500000;
      PE_SCN_ALIGN_32BYTES=$00600000;
      PE_SCN_ALIGN_64BYTES=$00700000;
      PE_SCN_LNK_NRELOC_OVFL=$01000000;
      PE_SCN_MEM_DISCARDABLE=$02000000;
      PE_SCN_MEM_NOT_CACHED=$04000000;
      PE_SCN_MEM_NOT_PAGED=$08000000;
      PE_SCN_MEM_SHARED=$10000000;
      PE_SCN_MEM_EXECUTE=$20000000;
      PE_SCN_MEM_READ=$40000000;
      PE_SCN_MEM_WRITE=TPACCUInt32($80000000);

      PECOFFSectionAlignment=$1000;
      PECOFFFileAlignment=$200;

type TBytes=array of TPACCUInt8;

     PPOINTER=^pointer;

     PPPACCUInt32=^PPACCUInt32;

     PPPACCUInt16=^PPACCUInt16;

     HINST=TPACCUInt32;
     HMODULE=HINST;

     PWordArray=^TWordArray;
     TWordArray=array[0..(2147483647 div SizeOf(TPACCUInt16))-1] of TPACCUInt16;

     PLongWordArray=^TLongWordArray;
     TLongWordArray=array [0..(2147483647 div SizeOf(TPACCUInt32))-1] of TPACCUInt32;

     PPECOFFDirectoryEntry=^TPECOFFDirectoryEntry;
     TPECOFFDirectoryEntry=record
      Data:TBytes;
      Position:TPACCUInt64;
      Size:TPACCUInt64;
     end;

     PPECOFFDirectoryEntries=^TPECOFFDirectoryEntries;
     TPECOFFDirectoryEntries=array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TPECOFFDirectoryEntry;

     TMZEXEHeader=packed record
      Signature:TPACCUInt16; // 00
      PartPag:TPACCUInt16;   // 02
      PageCnt:TPACCUInt16;   // 04
      ReloCnt:TPACCUInt16;   // 06
      HdrSize:TPACCUInt16;   // 08
      MinMem:TPACCUInt16;    // 0a
      MaxMem:TPACCUInt16;    // 0c
      ReloSS:TPACCUInt16;    // 0e
      ExeSP:TPACCUInt16;     // 10
      ChkSum:TPACCUInt16;    // 12
      ExeIP:TPACCUInt16;     // 14
      ReloCS:TPACCUInt16;    // 16
      TablOff:TPACCUInt16;   // 18
      Overlay:TPACCUInt16;   // 1a
     end;

     PImageDOSHeader=^TImageDOSHeader;
     TImageDOSHeader=packed record
      Signature:TPACCUInt16; // 00
      PartPag:TPACCUInt16;   // 02
      PageCnt:TPACCUInt16;   // 04
      ReloCnt:TPACCUInt16;   // 06
      HdrSize:TPACCUInt16;   // 08
      MinMem:TPACCUInt16;    // 0a
      MaxMem:TPACCUInt16;    // 0c
      ReloSS:TPACCUInt16;    // 0e
      ExeSP:TPACCUInt16;     // 10
      ChkSum:TPACCUInt16;    // 12
      ExeIP:TPACCUInt16;     // 14
      ReloCS:TPACCUInt16;    // 16
      TablOff:TPACCUInt16;   // 18
      Overlay:TPACCUInt16;   // 1a
      Reserved:packed array[0..3] of TPACCUInt16;
      OEMID:TPACCUInt16;
      OEMInfo:TPACCUInt16;
      Reserved2:packed array[0..9] of TPACCUInt16;
      LFAOffset:TPACCUInt32;
     end;

     TISHMisc=packed record
      case TPACCInt32 of
       0:(PhysicalAddress:TPACCUInt32);
       1:(VirtualSize:TPACCUInt32);
     end;

     PImageExportDirectory=^TImageExportDirectory;
     TImageExportDirectory=packed record
      Characteristics:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      MajorVersion:TPACCUInt16;
      MinorVersion:TPACCUInt16;
      Name:TPACCUInt32;
      Base:TPACCUInt32;
      NumberOfFunctions:TPACCUInt32;
      NumberOfNames:TPACCUInt32;
      AddressOfFunctions:PPPACCUInt32;
      AddressOfNames:PPPACCUInt32;
      AddressOfNameOrdinals:PPPACCUInt16;
     end;

     PImageSectionHeader=^TImageSectionHeader;
     TImageSectionHeader=packed record
      Name:packed array[0..IMAGE_SIZEOF_SHORT_NAME-1] of TPACCUInt8;
      Misc:TISHMisc;
      VirtualAddress:TPACCUInt32;
      SizeOfRawData:TPACCUInt32;
      PointerToRawData:TPACCUInt32;
      PointerToRelocations:TPACCUInt32;
      PointerToLineNumbers:TPACCUInt32;
      NumberOfRelocations:TPACCUInt16;
      NumberOfLineNumbers:TPACCUInt16;
      Characteristics:TPACCUInt32;
     end;

     PImageSectionHeaders=^TImageSectionHeaders;
     TImageSectionHeaders=array[0..(2147483647 div SizeOf(TImageSectionHeader))-1] of TImageSectionHeader;

     PImageDataDirectory=^TImageDataDirectory;
     TImageDataDirectory=packed record
      VirtualAddress:TPACCUInt32;
      Size:TPACCUInt32;
     end;

     PImageFileHeader=^TImageFileHeader;
     TImageFileHeader=packed record
      Machine:TPACCUInt16;
      NumberOfSections:TPACCUInt16;
      TimeDateStamp:TPACCUInt32;
      PointerToSymbolTable:TPACCUInt32;
      NumberOfSymbols:TPACCUInt32;
      SizeOfOptionalHeader:TPACCUInt16;
      Characteristics:TPACCUInt16;
     end;

     PImageOptionalHeader=^TImageOptionalHeader;
     TImageOptionalHeader=packed record
      Magic:TPACCUInt16;
      MajorLinkerVersion:TPACCUInt8;
      MinorLinkerVersion:TPACCUInt8;
      SizeOfCode:TPACCUInt32;
      SizeOfInitializedData:TPACCUInt32;
      SizeOfUninitializedData:TPACCUInt32;
      AddressOfEntryPoint:TPACCUInt32;
      BaseOfCode:TPACCUInt32;
      BaseOfData:TPACCUInt32;
      ImageBase:TPACCUInt32;
      SectionAlignment:TPACCUInt32;
      FileAlignment:TPACCUInt32;
      MajorOperatingSystemVersion:TPACCUInt16;
      MinorOperatingSystemVersion:TPACCUInt16;
      MajorImageVersion:TPACCUInt16;
      MinorImageVersion:TPACCUInt16;
      MajorSubsystemVersion:TPACCUInt16;
      MinorSubsystemVersion:TPACCUInt16;
      Win32VersionValue:TPACCUInt32;
      SizeOfImage:TPACCUInt32;
      SizeOfHeaders:TPACCUInt32;
      CheckSum:TPACCUInt32;
      Subsystem:TPACCUInt16;
      DLLCharacteristics:TPACCUInt16;
      SizeOfStackReserve:TPACCUInt32;
      SizeOfStackCommit:TPACCUInt32;
      SizeOfHeapReserve:TPACCUInt32;
      SizeOfHeapCommit:TPACCUInt32;
      LoaderFlags:TPACCUInt32;
      NumberOfRvaAndSizes:TPACCUInt32;
      DataDirectory:packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
     end;

     PImageOptionalHeader64=^TImageOptionalHeader64;
     TImageOptionalHeader64=packed record
      Magic:TPACCUInt16;
      MajorLinkerVersion:TPACCUInt8;
      MinorLinkerVersion:TPACCUInt8;
      SizeOfCode:TPACCUInt32;
      SizeOfInitializedData:TPACCUInt32;
      SizeOfUninitializedData:TPACCUInt32;
      AddressOfEntryPoint:TPACCUInt32;
      BaseOfCode:TPACCUInt32;
      ImageBase:TPACCUInt64;
      SectionAlignment:TPACCUInt32;
      FileAlignment:TPACCUInt32;
      MajorOperatingSystemVersion:TPACCUInt16;
      MinorOperatingSystemVersion:TPACCUInt16;
      MajorImageVersion:TPACCUInt16;
      MinorImageVersion:TPACCUInt16;
      MajorSubsystemVersion:TPACCUInt16;
      MinorSubsystemVersion:TPACCUInt16;
      Win32VersionValue:TPACCUInt32;
      SizeOfImage:TPACCUInt32;
      SizeOfHeaders:TPACCUInt32;
      CheckSum:TPACCUInt32;
      Subsystem:TPACCUInt16;
      DLLCharacteristics:TPACCUInt16;
      SizeOfStackReserve:TPACCUInt64;
      SizeOfStackCommit:TPACCUInt64;
      SizeOfHeapReserve:TPACCUInt64;
      SizeOfHeapCommit:TPACCUInt64;
      LoaderFlags:TPACCUInt32;
      NumberOfRvaAndSizes:TPACCUInt32;
      DataDirectory:packed array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TImageDataDirectory;
     end;

     PImageNTHeaders=^TImageNTHeaders;
     TImageNTHeaders=packed record
      Signature:TPACCUInt32;
      FileHeader:TImageFileHeader;
      case boolean of
       false:(
        OptionalHeader:TImageOptionalHeader;
       );
       true:(
        OptionalHeader64:TImageOptionalHeader64;
       );
     end;

     PImageImportDescriptor=^TImageImportDescriptor;
     TImageImportDescriptor=packed record
      OriginalFirstThunk:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      ForwarderChain:TPACCUInt32;
      Name:TPACCUInt32;
      FirstThunk:TPACCUInt32;
     end;

     PImageBaseRelocation=^TImageBaseRelocation;
     TImageBaseRelocation=packed record
      VirtualAddress:TPACCUInt32;
      SizeOfBlock:TPACCUInt32;
     end;

     PImageThunkData=^TImageThunkData;
     TImageThunkData=packed record
      ForwarderString:TPACCUInt32;
      Funktion:TPACCUInt32;
      Ordinal:TPACCUInt32;
      AddressOfData:TPACCUInt32;
     end;

constructor TPACCLinker_COFF_PESection.Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const AVirtualAddress:TPACCUInt64;const ACharacteristics:TPACCUInt32);
begin
 inherited Create;

 fLinker:=ALinker;

 fName:=AName;

 fStream:=TMemoryStream.Create;

 fAlignment:=1;

 fVirtualAddress:=AVirtualAddress;

 fCharacteristics:=ACharacteristics;
 
 Relocations:=nil;

end;

destructor TPACCLinker_COFF_PESection.Destroy;
begin
 Relocations:=nil;
 fStream.Free;
 inherited Destroy;
end;

constructor TPACCLinker_COFF_PESectionList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PESectionList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_COFF_PESectionList.GetItem(const Index:TPACCInt):TPACCLinker_COFF_PESection;
begin
 result:=pointer(inherited Items[Index]);
end;

procedure TPACCLinker_COFF_PESectionList.SetItem(const Index:TPACCInt;Node:TPACCLinker_COFF_PESection);
begin
 inherited Items[Index]:=pointer(Node);
end;

constructor TPACCLinker_COFF_PESymbol.Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const ASection:TPACCLinker_COFF_PESection;const AValue:TPACCInt64;const AType,AClass:TPACCInt32);
begin
 inherited Create;
 fLinker:=ALinker;
 fName:=AName;
 fSection:=ASection;
 fValue:=AValue;
 fType:=AType;
 fClass:=AClass;
 fSubSymbols:=TPACCLinker_COFF_PESymbolList.Create;
end;

destructor TPACCLinker_COFF_PESymbol.Destroy;
begin
 while fSubSymbols.Count>0 do begin
  fSubSymbols[fSubSymbols.Count-1].Free;
  fSubSymbols.Delete(fSubSymbols.Count-1);
 end;
 fSubSymbols.Free;
 inherited Destroy;
end;

constructor TPACCLinker_COFF_PESymbolList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PESymbolList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_COFF_PESymbolList.GetItem(const Index:TPACCInt):TPACCLinker_COFF_PESymbol;
begin
 result:=pointer(inherited Items[Index]);
end;

procedure TPACCLinker_COFF_PESymbolList.SetItem(const Index:TPACCInt;Node:TPACCLinker_COFF_PESymbol);
begin
 inherited Items[Index]:=pointer(Node);
end;

constructor TPACCLinker_COFF_PE.Create(const AInstance:TObject);
begin
 inherited Create(AInstance);

 if TPACCInstance(Instance).Target is TPACCTarget_x86_32 then begin
  fMachine:=IMAGE_FILE_MACHINE_I386;
{end else if TPACCInstance(Instance).Target is TPACCTarget_x86_64_Win64 then begin
  fMachine:=IMAGE_FILE_MACHINE_AMD64;}
 end else begin
  fMachine:=IMAGE_FILE_MACHINE_UNKNOWN;
 end;

 fSections:=TPACCLinker_COFF_PESectionList.Create;

 fSymbols:=TPACCLinker_COFF_PESymbolList.Create;

end;

destructor TPACCLinker_COFF_PE.Destroy;
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

procedure TPACCLinker_COFF_PE.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
const COFF_SIZEOF_SHORT_NAME=8;
type PCOFFFileHeader=^TCOFFFileHeader;
     TCOFFFileHeader=packed record
      Machine:TPACCUInt16;
      NumberOfSections:TPACCUInt16;
      TimeDateStamp:TPACCUInt32;
      PointerToSymbolTable:TPACCUInt32;
      NumberOfSymbols:TPACCUInt32;
      SizeOfOptionalHeader:TPACCUInt16;
      Characteristics:TPACCUInt16;
     end;
     PCOFFSectionHeader=^TCOFFSectionHeader;
     TCOFFSectionHeader=packed record
      Name:packed array[0..COFF_SIZEOF_SHORT_NAME-1] of ansichar;
      VirtualSize:TPACCUInt32;
      VirtualAddress:TPACCUInt32;
      SizeOfRawData:TPACCUInt32;
      PointerToRawData:TPACCUInt32;
      PointerToRelocations:TPACCUInt32;
      PointerToLineNumbers:TPACCUInt32;
      NumberOfRelocations:TPACCUInt16;
      NumberOfLineNumbers:TPACCUInt16;
      Characteristics:TPACCUInt32;
     end;
     TCOFFSectionHeaders=array of TCOFFSectionHeader;
     PCOFFSymbolName=^TCOFFSymbolName;
     TCOFFSymbolName=packed record
      case TPACCInt32 of
       0:(
        Name:packed array[0..7] of ansichar;
       );
       1:(
        Zero:TPACCUInt32;
        PointerToString:TPACCUInt32;
       );
     end;
     PCOFFSymbol=^TCOFFSymbol;
     TCOFFSymbol=packed record
      Name:TCOFFSymbolName;
      Value:TPACCUInt32;
      Section:TPACCUInt16;
      SymbolType:TPACCUInt16;
      SymbolClass:TPACCUInt8;
      Aux:TPACCUInt8;
     end;
     TCOFFSymbols=array of TCOFFSymbol;
     PCOFFRelocation=^TCOFFRelocation;
     TCOFFRelocation=packed record
      VirtualAddress:TPACCUInt32;
      Symbol:TPACCUInt32;
      RelocationType:TPACCUInt16;
     end;
     TCOFFRelocations=array of TCOFFRelocation;
     PSymbolTargetStackItem=^TSymbolTargetStackItem;
     TSymbolTargetStackItem=record
      SymbolTarget:TPACCLinker_COFF_PESymbolList;
      Remaining:TPACCInt32;
     end;
     TSymbolTargetStackItems=array of TSymbolTargetStackItem;
var SectionIndex,RelocationIndex,NumberOfRelocations,SymbolIndex,Index,SymbolTargetStackPointer:TPACCInt32;
    RelocationOffset:TPACCUInt32;
    COFFFileHeader:TCOFFFileHeader;
    LocalSections:TPACCLinker_COFF_PESectionList;
    COFFSectionHeaders:TCOFFSectionHeaders;
    COFFSectionHeader:PCOFFSectionHeader;
    Section:TPACCLinker_COFF_PESection;
    OldSize:TPACCInt64;
    COFFRelocations:TCOFFRelocations;
    COFFRelocation:PCOFFRelocation;
    Relocation:PPACCLinker_COFF_PERelocation;
    COFFSymbols:TCOFFSymbols;
    COFFSymbol:PCOFFSymbol;
    Symbol:TPACCLinker_COFF_PESymbol;
    SymbolTargetStackItems:TSymbolTargetStackItems;
    SymbolTargetStackItem:PSymbolTargetStackItem;
    Name:TPACCRawByteString;
    c:ansichar;
begin

 COFFSectionHeaders:=nil;
 try

  if AObjectStream.Seek(0,soBeginning)<>0 then begin
   TPACCInstance(Instance).AddError('Stream seek error',nil,true);
  end;

  AObjectStream.ReadBuffer(COFFFileHeader,SizeOf(TCOFFFileHeader));

  case COFFFileHeader.Machine of
   IMAGE_FILE_MACHINE_I386:begin
    if (COFFFileHeader.Characteristics and IMAGE_FILE_32BIT_MACHINE)=0 then begin
     TPACCInstance(Instance).AddError('Unsupported COFF machine type',nil,true);
    end;
   end;
{  IMAGE_FILE_MACHINE_R3000:begin
   end;
   IMAGE_FILE_MACHINE_R4000:begin
   end;
   IMAGE_FILE_MACHINE_R10000:begin
   end;
   IMAGE_FILE_MACHINE_ALPHA:begin
   end;
   IMAGE_FILE_MACHINE_POWERPC:begin
   end;}
   IMAGE_FILE_MACHINE_AMD64:begin
    if (COFFFileHeader.Characteristics and IMAGE_FILE_32BIT_MACHINE)<>0 then begin
     TPACCInstance(Instance).AddError('Unsupported COFF machine type',nil,true);
    end;
   end;
   else begin
    TPACCInstance(Instance).AddError('Unsupported COFF machine type',nil,true);
   end;
  end;

  if COFFFileHeader.Machine<>fMachine then begin
   TPACCInstance(Instance).AddError('Wrong COFF machine type',nil,true);
  end;

  if COFFFileHeader.NumberOfSections=0 then begin
   TPACCInstance(Instance).AddError('No COFF sections',nil,true);
  end;

  SetLength(COFFSectionHeaders,COFFFileHeader.NumberOfSections);
  AObjectStream.ReadBuffer(COFFSectionHeaders[0],COFFFileHeader.NumberOfSections*SizeOf(TCOFFSectionHeader));

  LocalSections:=TPACCLinker_COFF_PESectionList.Create;
  try

   for SectionIndex:=0 to COFFFileHeader.NumberOfSections-1 do begin
    COFFSectionHeader:=@COFFSectionHeaders[SectionIndex];
    if COFFSectionHeader^.VirtualSize>0 then begin
     Section:=TPACCLinker_COFF_PESection.Create(self,COFFSectionHeader^.Name,COFFSectionHeader^.VirtualAddress,COFFSectionHeader^.Characteristics);
     Sections.Add(Section);
     LocalSections.Add(Section);
     if (COFFSectionHeader^.Characteristics and IMAGE_SCN_ALIGN_MASK)<>0 then begin
      Section.Alignment:=1 shl (((COFFSectionHeader^.Characteristics and IMAGE_SCN_ALIGN_MASK) shr IMAGE_SCN_ALIGN_SHIFT)-1);
     end;
     if (COFFSectionHeader^.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA)=0 then begin
      if COFFSectionHeader^.SizeOfRawData>0 then begin
       if COFFSectionHeader^.SizeOfRawData>COFFSectionHeader^.VirtualSize then begin
        TPACCInstance(Instance).AddError('SizeOfRawData is larger than VirtualSize at section "'+Section.Name+'"',nil,true);
       end;
       if AObjectStream.Seek(COFFSectionHeader^.PointerToRawData,soBeginning)<>COFFSectionHeader^.PointerToRawData then begin
        TPACCInstance(Instance).AddError('Stream seek error',nil,true);
       end;
       if Section.Stream.CopyFrom(AObjectStream,COFFSectionHeader^.SizeOfRawData)<>COFFSectionHeader^.SizeOfRawData then begin
        TPACCInstance(Instance).AddError('Stream read error',nil,true);
       end;
      end;
     end;
     if Section.Stream.Size<COFFSectionHeader^.VirtualSize then begin
      OldSize:=Section.Stream.Size;
      Section.Stream.SetSize(COFFSectionHeader^.VirtualSize);
      if Section.Stream.Size<>COFFSectionHeader^.VirtualSize then begin
       TPACCInstance(Instance).AddError('Stream resize error',nil,true);
      end;
      FillChar(PAnsiChar(Section.Stream.Memory)[OldSize],COFFSectionHeader^.VirtualSize-OldSize,#0);
     end;
     if COFFSectionHeader^.PointerToRelocations>0 then begin
      if (COFFSectionHeader^.NumberOfRelocations=IMAGE_SCN_MAX_RELOC) and
         ((COFFSectionHeader^.Characteristics and IMAGE_SCN_LNK_NRELOC_OVFL)<>0) then begin
       COFFRelocations:=nil;
       try
        SetLength(COFFRelocations,1);
        if AObjectStream.Seek(COFFSectionHeader^.PointerToRelocations,soBeginning)<>COFFSectionHeader^.PointerToRelocations then begin
         TPACCInstance(Instance).AddError('Stream seek error',nil,true);
        end;
        AObjectStream.ReadBuffer(COFFRelocations[0],SizeOf(TCOFFRelocation));
        NumberOfRelocations:=COFFRelocations[0].VirtualAddress;
        RelocationOffset:=SizeOf(TCOFFRelocation);
       finally
        COFFRelocations:=nil;
       end;
      end else begin
       NumberOfRelocations:=COFFSectionHeader^.NumberOfRelocations;
       RelocationOffset:=0;
      end;
      if NumberOfRelocations>0 then begin
       COFFRelocations:=nil;
       try
        SetLength(COFFRelocations,NumberOfRelocations);
        if AObjectStream.Seek(COFFSectionHeader^.PointerToRelocations+RelocationOffset,soBeginning)<>(COFFSectionHeader^.PointerToRelocations+RelocationOffset) then begin
         TPACCInstance(Instance).AddError('Stream seek error',nil,true);
        end;
        AObjectStream.ReadBuffer(COFFRelocations[0],NumberOfRelocations*SizeOf(TCOFFRelocation));
        SetLength(Section.Relocations,NumberOfRelocations);
        for RelocationIndex:=0 to NumberOfRelocations-1 do begin
         COFFRelocation:=@COFFRelocations[RelocationIndex];
         Relocation:=@Section.Relocations[RelocationIndex];
         Relocation^.VirtualAddress:=COFFRelocation^.VirtualAddress;
         Relocation^.Symbol:=COFFRelocation^.Symbol;
         Relocation^.RelocationType:=COFFRelocation^.RelocationType;
        end;
       finally
        COFFRelocations:=nil;
       end;
      end;
     end;
    end else begin
     LocalSections.Add(nil);
    end;
   end;

   if (COFFFileHeader.PointerToSymbolTable>0) and (COFFFileHeader.NumberOfSymbols>0) then begin
    SymbolTargetStackItems:=nil;
    try
     SetLength(SymbolTargetStackItems,16);
     SymbolTargetStackItem:=@SymbolTargetStackItems[0];
     SymbolTargetStackItem^.SymbolTarget:=Symbols;
     SymbolTargetStackItem^.Remaining:=-1;
     SymbolTargetStackPointer:=0;
     COFFSymbols:=nil;
     try
      SetLength(COFFSymbols,COFFFileHeader.NumberOfSymbols);
      if AObjectStream.Seek(COFFFileHeader.PointerToSymbolTable,soBeginning)<>COFFFileHeader.PointerToSymbolTable then begin
       TPACCInstance(Instance).AddError('Stream seek error',nil,true);
      end;
      AObjectStream.ReadBuffer(COFFSymbols[0],COFFFileHeader.NumberOfSymbols*SizeOf(TCOFFSymbol));
      for SymbolIndex:=0 to COFFFileHeader.NumberOfSymbols-1 do begin
       COFFSymbol:=@COFFSymbols[SymbolIndex];
       if COFFSymbol^.Name.Zero=0 then begin
        if AObjectStream.Seek(COFFSymbol^.Name.PointerToString,soBeginning)<>COFFSymbol^.Name.PointerToString then begin
         TPACCInstance(Instance).AddError('Stream seek error',nil,true);
        end;
        Name:='';
        while AObjectStream.Position<AObjectStream.Size do begin
         AObjectStream.Read(c,SizeOf(AnsiChar));
         if c=#0 then begin
          break;
         end else begin
          Name:=Name+c;
         end;
        end;
       end else begin
        Name:=COFFSymbol^.Name.Name;
        for Index:=1 to length(Name) do begin
         if Name[Index]=#0 then begin
          Name:=copy(Name,1,Index-1);
          break;
         end;
        end;
       end;
       if (COFFSymbol^.Section>0) and (COFFSymbol^.Section<=LocalSections.Count) then begin
        Section:=LocalSections[COFFSymbol^.Section-1];
       end else begin
        Section:=nil;
       end;
       Symbol:=TPACCLinker_COFF_PESymbol.Create(self,Name,Section,COFFSymbol^.Value,COFFSymbol^.SymbolType,COFFSymbol^.SymbolClass);
       SymbolTargetStackItem:=@SymbolTargetStackItems[SymbolTargetStackPointer];
       SymbolTargetStackItem^.SymbolTarget.Add(Symbol);
       if SymbolTargetStackItem^.Remaining>0 then begin
        dec(SymbolTargetStackItem^.Remaining);
        if SymbolTargetStackItem^.Remaining=0 then begin
         dec(SymbolTargetStackPointer);
        end;
       end;
       if COFFSymbol^.Aux>0 then begin
        inc(SymbolTargetStackPointer);
        if length(SymbolTargetStackItems)<=SymbolTargetStackPointer then begin
         SetLength(SymbolTargetStackItems,(SymbolTargetStackPointer+1)*2);
        end;
        SymbolTargetStackItem:=@SymbolTargetStackItems[SymbolTargetStackPointer];
        SymbolTargetStackItem^.SymbolTarget:=Symbol.SubSymbols;
        SymbolTargetStackItem^.Remaining:=COFFSymbol^.Aux;
       end;
      end;
     finally
      COFFSymbols:=nil;
     end;
    finally
     SymbolTargetStackItems:=nil;
    end;
   end;

  finally
   LocalSections.Free;
  end;

 finally
  COFFSectionHeaders:=nil;
 end;
end;

procedure TPACCLinker_COFF_PE.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
begin
end;


end.
