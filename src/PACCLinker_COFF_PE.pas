unit PACCLinker_COFF_PE;
{$i PACC.inc}

interface

uses SysUtils,Classes,Math,PUCU,PACCTypes,PACCGlobals,PACCRawByteStringHashMap,PACCPointerHashMap,PACCLinker;

type TPACCLinker_COFF_PE=class;

     PPACCLinker_COFF_PE_Relocation=^TPACCLinker_COFF_PE_Relocation;
     TPACCLinker_COFF_PE_Relocation=record
      VirtualAddress:TPACCUInt32;
      Symbol:TPACCUInt32;
      RelocationType:TPACCUInt16;
     end;

     TPACCLinker_COFF_PE_Relocations=array of TPACCLinker_COFF_PE_Relocation;

     TPACCLinker_COFF_PE_SymbolList=class;

     TPACCLinker_COFF_PE_Section=class
      private
       fLinker:TPACCLinker_COFF_PE;
       fName:TPACCRawByteString;
       fOrdering:TPACCRawByteString;
       fOrder:TPACCInt32;
       fStream:TMemoryStream;
       fAlignment:TPACCInt32;
       fVirtualAddress:TPACCUInt64;
       fVirtualSize:TPACCUInt64;
       fRawSize:TPACCUInt64;
       fCharacteristics:TPACCUInt32;
       fSymbols:TPACCLinker_COFF_PE_SymbolList;
       fFileOffset:TPACCInt64;
       fActive:boolean;
      protected
       Relocations:TPACCLinker_COFF_PE_Relocations;
       CountRelocations:TPACCInt32;
       function NewRelocation:PPACCLinker_COFF_PE_Relocation;
      public
       constructor Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const AVirtualAddress:TPACCUInt64;const ACharacteristics:TPACCUInt32); reintroduce;
       destructor Destroy; override;
      published
       property Linker:TPACCLinker_COFF_PE read fLinker;
       property Name:TPACCRawByteString read fName write fName;
       property Ordering:TPACCRawByteString read fOrdering write fOrdering;
       property Order:TPACCInt32 read fOrder write fOrder;
       property Stream:TMemoryStream read fStream;
       property Alignment:TPACCInt32 read fAlignment write fAlignment;
       property VirtualAddress:TPACCUInt64 read fVirtualAddress write fVirtualAddress;
       property VirtualSize:TPACCUInt64 read fVirtualSize write fVirtualSize;
       property RawSize:TPACCUInt64 read fRawSize write fRawSize;
       property Characteristics:TPACCUInt32 read fCharacteristics write fCharacteristics;
       property Symbols:TPACCLinker_COFF_PE_SymbolList read fSymbols;
       property FileOffset:TPACCInt64 read fFileOffset write fFileOffset;
       property Active:boolean read fActive write fActive;
     end;

     TPACCLinker_COFF_PE_SectionList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Section;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Section);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_COFF_PE_Section read GetItem write SetItem; default;
     end;

     PPACCLinker_COFF_PE_SymbolKind=^TPACCLinker_COFF_PE_SymbolKind;
     TPACCLinker_COFF_PE_SymbolKind=
      (
       plcpskUndefined,
       plcpskAbsolute,
       plcpskDebug,
       plcpskNormal
      );

     TPACCLinker_COFF_PE_Symbol=class
      private
       fLinker:TPACCLinker_COFF_PE;
       fName:TPACCRawByteString;
       fSection:TPACCLinker_COFF_PE_Section;
       fValue:TPACCInt64;
       fType:TPACCInt32;
       fClass:TPACCInt32;
       fSymbolKind:TPACCLinker_COFF_PE_SymbolKind;
       fAlias:TPACCLinker_COFF_PE_Symbol;
       fSubSymbols:TPACCLinker_COFF_PE_SymbolList;
       fAuxData:TMemoryStream;
       fActive:boolean;
      public
       constructor Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const ASection:TPACCLinker_COFF_PE_Section;const AValue:TPACCInt64;const AType,AClass:TPACCInt32;const ASymbolKind:TPACCLinker_COFF_PE_SymbolKind); reintroduce;
       destructor Destroy; override;
      published
       property Linker:TPACCLinker_COFF_PE read fLinker;
       property Name:TPACCRawByteString read fName;
       property Section:TPACCLinker_COFF_PE_Section read fSection write fSection;
       property Value:TPACCInt64 read fValue write fValue;
       property Type_:TPACCInt32 read fType;
       property Class_:TPACCInt32 read fClass;
       property SymbolKind:TPACCLinker_COFF_PE_SymbolKind read fSymbolKind write fSymbolKind;
       property Alias:TPACCLinker_COFF_PE_Symbol read fAlias write fAlias;
       property SubSymbols:TPACCLinker_COFF_PE_SymbolList read fSubSymbols;
       property AuxData:TMemoryStream read fAuxData write fAuxData;
       property Active:boolean read fActive write fActive;
     end;

     TPACCLinker_COFF_PE_SymbolList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Symbol;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Symbol);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_COFF_PE_Symbol read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PE_Resource=class
      private
       fLinker:TPACCLinker_COFF_PE;
       fType_:TPUCUUTF16String;
       fName:TPUCUUTF16String;
       fDataVersion:TPACCUInt32;
       fMemoryFlags:TPACCUInt16;
       fLanguageID:TPACCUInt16;
       fVersion:TPACCUInt32;
       fCharacteristics:TPACCUInt32;
       fCodePage:TPACCUInt32;
       fStream:TMemoryStream;
      public
       constructor Create(const ALinker:TPACCLinker_COFF_PE); reintroduce;
       destructor Destroy; override;
       procedure UpdateCodePage; 
      published
       property Linker:TPACCLinker_COFF_PE read fLinker;
       property Type_:TPUCUUTF16String read fType_ write fType_;
       property Name:TPUCUUTF16String read fName write fName;
       property DataVersion:TPACCUInt32 read fDataVersion write fDataVersion;
       property MemoryFlags:TPACCUInt16 read fMemoryFlags write fMemoryFlags;
       property LanguageID:TPACCUInt16 read fLanguageID write fLanguageID;
       property Version:TPACCUInt32 read fVersion write fVersion;
       property Characteristics:TPACCUInt32 read fCharacteristics write fCharacteristics;
       property CodePage:TPACCUInt32 read fCodePage write fCodePage;
       property Stream:TMemoryStream read fStream;
     end;

     TPACCLinker_COFF_PE_ResourceList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Resource;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Resource);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_COFF_PE_Resource read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PE_Import=class
      public
       Used:boolean;
       SymbolName:TPUCUUTF8String;
       ImportLibraryName:TPUCUUTF8String;
       ImportName:TPUCUUTF8String;
       CodeSectionOffset:TPACCUInt64;
       NameOffset:TPACCUInt64;
     end;

     TPACCLinker_COFF_PE_ImportList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Import;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Import);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_COFF_PE_Import read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PE_Export=class
      public
       Used:boolean;
       SymbolName:TPUCUUTF8String;
       ExportName:TPUCUUTF8String;
     end;

     TPACCLinker_COFF_PE_ExportList=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Export;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Export);
      public
       constructor Create;
       destructor Destroy; override;
       property Items[const AIndex:TPACCInt]:TPACCLinker_COFF_PE_Export read GetItem write SetItem; default;
     end;

     TPACCLinker_COFF_PE=class(TPACCLinker)
      private

       fMachine:TPACCUInt16;

       fSections:TPACCLinker_COFF_PE_SectionList;

       fSymbols:TPACCLinker_COFF_PE_SymbolList;

       fImports:TPACCLinker_COFF_PE_ImportList;
       fImportSymbolNameHashMap:TPACCRawByteStringHashMap;

       fExports:TPACCLinker_COFF_PE_ExportList;
       fExportSymbolNameHashMap:TPACCRawByteStringHashMap;

       fResources:TPACCLinker_COFF_PE_ResourceList;

       fImageBase:TPACCUInt64;

      public

       constructor Create(const AInstance:TObject); override;
       destructor Destroy; override;

       procedure AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String); override;

       procedure AddExport(const ASymbolName,AExportName:TPUCUUTF8String); override;

       procedure AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String=''); override;

       procedure AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String=''); override;

       procedure Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String=''); override;

      published

       property Machine:TPACCUInt16 read fMachine;

       property Sections:TPACCLinker_COFF_PE_SectionList read fSections;

       property Symbols:TPACCLinker_COFF_PE_SymbolList read fSymbols;

       property Imports_:TPACCLinker_COFF_PE_ImportList read fImports;

       property Exports_:TPACCLinker_COFF_PE_ExportList read fExports;

       property Resources:TPACCLinker_COFF_PE_ResourceList read fResources;

       property ImageBase:TPACCUInt64 read fImageBase write fImageBase;

     end;

implementation

uses PACCInstance,PACCLinker_COFF_PE_LCIDToCodePageLookUpTable,PACCTarget_x86_32{,PACCTarget_x86_64_Win64};

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
      IMAGE_FILE_LARGE_ADDRESS_AWARE=$0020;
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
      IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT_DESCRIPTOR=13;
      IMAGE_DIRECTORY_ENTRY_CLR_RUNTIME_HEADER=14;

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
      IMAGE_SCN_CNT_RESOURCE:TPACCInt64=$100000000;

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
      IMAGE_REL_AMD64_ADDR64NB=$ffff; // Only for internal usage

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

      IMAGE_SYM_UNDEFINED=0;
      IMAGE_SYM_ABSOLUTE=$ffff;
      IMAGE_SYM_DEBUG=$fffe;

      PECOFFSectionAlignment=$1000;
      PECOFFFileAlignment=$200;

      COFF_SIZEOF_SHORT_NAME=8;

      RT_CURSOR=1;
      RT_BITMAP=2;
      RT_ICON=3;
      RT_MENU=4;
      RT_DIALOG=5;
      RT_STRING=6;
      RT_FONTDIR=7;
      RT_FONT=8;
      RT_ACCELERATOR=9;
      RT_RCDATA=10;
      RT_MESSAGETABLE=11;
      RT_DIFFERENCE=11;
      RT_GROUP_CURSOR=RT_CURSOR+RT_DIFFERENCE;
      RT_GROUP_ICON=RT_ICON+RT_DIFFERENCE;
      RT_VERSION=16;
      RT_DLGINCLUDE=17;
      RT_PLUGPLAY=19;
      RT_VXD=20;
      RT_ANICURSOR=21;
      RT_ANIICON=22;
      RT_MANIFEST=24;

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
      AddressOfFunctions:TPACCUInt32;
      AddressOfNames:TPACCUInt32;
      AddressOfNameOrdinals:TPACCUInt32;
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

     PImageImportDescriptors=^TImageImportDescriptors;
     TImageImportDescriptors=array[0..65535] of TImageImportDescriptor;

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

     PImageResourceDirectory=^TImageResourceDirectory;
     TImageResourceDirectory=packed record 
      Characteristics:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      MajorVersion:TPACCUInt16;
      MinorVersion:TPACCUInt16;            
      NumberOfNamedEntries:TPACCUInt16;
      NumberOfIDEntries:TPACCUInt16;
     end;

     PImageResourceDirectoryEntry=^TImageResourceDirectoryEntry;
     TImageResourceDirectoryEntry=packed record
      TypeNameLevel:TPACCUInt32;
      OffsetToChildData:TPACCUInt32;
     end;

     PImageResourceDataEntry=^TImageResourceDataEntry;
     TImageResourceDataEntry=packed record
      OffsetToData:TPACCUInt32;
      Size:TPACCUInt32;
      CodePage:TPACCUInt32;
      Reserved:TPACCUInt32;
     end;

     PImageResourceDirStringU=^TImageResourceDirStringU;
     TImageResourceDirStringU=packed record
      length:TPACCUInt16;
      NameString:array[0..0] of TPUCUUTF16Char;
     end;

     PImageDebugDirectory=^TImageDebugDirectory;
     TImageDebugDirectory=packed record
      Characteristics:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      MajorVersion:TPACCUInt16;
      MinorVersion:TPACCUInt16;
      DebugType:TPACCUInt32;
      SizeOfData:TPACCUInt32;
      AddressOfRawData:TPACCUInt32;
      PointerToRawData:TPACCUInt32;
     end;

     PImageTLSDirectory32=^TImageTLSDirectory32;
     TImageTLSDirectory32=packed record
      StartAddressOfRawData:TPACCUInt32;
      EndAddressOfRawData:TPACCUInt32;
      AddressOfIndex:TPACCUInt32;
      AddressOfCallBacks:TPACCUInt32;
      SizeOfZeroFill:TPACCUInt32;
      Characteristics:TPACCUInt32;
     end;

     PImageTLSDirectory64=^TImageTLSDirectory64;
     TImageTLSDirectory64=packed record
      StartAddressOfRawData:TPACCUInt64;
      EndAddressOfRawData:TPACCUInt64;
      AddressOfIndex:TPACCUInt64;
      AddressOfCallBacks:TPACCUInt64;
      SizeOfZeroFill:TPACCUInt32;
      Characteristics:TPACCUInt32;
     end;

     PImageDelayDescriptor=^TImageDelayDescriptor;
     TImageDelayDescriptor=packed record
      grArrs:TPACCUInt32;
      rvaDLLName:TPACCUInt32;
      rvaHmod:TPACCUInt32;
      rvaIAT:TPACCUInt32;
      rvaINT:TPACCUInt32;
      rvaBoundIAT:TPACCUInt32;
      rvaUnloadIAT:TPACCUInt32;
      dwTimeStamp:TPACCUInt32;
     end;

     PImageLoadConfiguration32=^TImageLoadConfiguration32;
     TImageLoadConfiguration32=packed record
      Characteristics:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      MajorVersion:TPACCUInt16;
      MinorVersion:TPACCUInt16;
      GlobalFlagsClear:TPACCUInt32;
      GlobalFlagsSet:TPACCUInt32;
      CriticalSectionDefaultTimeout:TPACCUInt32;
      DeCommitFreeBlockThreshold:TPACCUInt32;
      DeCommitTotalFreeThreshold:TPACCUInt32;
      LockPrefixTable:TPACCUInt32;
      MaximumAllocationSize:TPACCUInt32;
      VirtualMemoryThreshold:TPACCUInt32;
      ProcessAffinityMask:TPACCUInt32;
      ProcessHeapFlags:TPACCUInt32;
      CSDVersion:TPACCUInt16;
      Reserved:TPACCUInt16;
      EditList:TPACCUInt32;
      SecurityCookie:TPACCUInt32;
      SEHandlerTable:TPACCUInt32;
      SEHandlerCount:TPACCUInt32;
     end;

     PImageLoadConfiguration64=^TImageLoadConfiguration64;
     TImageLoadConfiguration64=packed record
      Characteristics:TPACCUInt32;
      TimeDateStamp:TPACCUInt32;
      MajorVersion:TPACCUInt16;
      MinorVersion:TPACCUInt16;
      GlobalFlagsClear:TPACCUInt32;
      GlobalFlagsSet:TPACCUInt32;
      CriticalSectionDefaultTimeout:TPACCUInt32;
      DeCommitFreeBlockThreshold:TPACCUInt32;
      DeCommitTotalFreeThreshold:TPACCUInt32;
      LockPrefixTable:TPACCUInt32;
      MaximumAllocationSize:TPACCUInt32;
      VirtualMemoryThreshold:TPACCUInt32;
      ProcessAffinityMask:TPACCUInt32;
      ProcessHeapFlags:TPACCUInt32;
      CSDVersion:TPACCUInt16;
      Reserved:TPACCUInt16;
      EditList:TPACCUInt32;
      SecurityCookie:TPACCUInt64;
      SEHandlerTable:TPACCUInt64;
      SEHandlerCount:TPACCUInt64;
     end;

     PCOFFFileHeader=^TCOFFFileHeader;
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

     PImageAuxSymbol=^TImageAuxSymbol;
     TImageAuxSymbol=packed record
      case TPACCUInt32 of
       IMAGE_SYM_CLASS_END_OF_FUNCTION:(
       );
       IMAGE_SYM_CLASS_NULL:(
       );
       IMAGE_SYM_CLASS_AUTOMATIC:(
       );
       IMAGE_SYM_CLASS_EXTERNAL:(
        FunctionDefinition:packed record
         TagIndex:TPACCUInt32;
         TotalSize:TPACCUInt32;
         PointerToLineNumber:TPACCUInt32;
         PointerToNextFunction:TPACCUInt32;
        end;
       );
       IMAGE_SYM_CLASS_STATIC:(
       );
       IMAGE_SYM_CLASS_REGISTER:(
       );
       IMAGE_SYM_CLASS_EXTERNAL_DEF:(
       );
       IMAGE_SYM_CLASS_LABEL:(
       );
       IMAGE_SYM_CLASS_UNDEFINED_LABEL:(
       );
       IMAGE_SYM_CLASS_MEMBER_OF_STRUCT:(
       );
       IMAGE_SYM_CLASS_ARGUMENT:(
       );
       IMAGE_SYM_CLASS_STRUCT_TAG:(
       );
       IMAGE_SYM_CLASS_MEMBER_OF_UNION:(
       );
       IMAGE_SYM_CLASS_UNION_TAG:(
       );
       IMAGE_SYM_CLASS_TYPE_DEFINITION:(
       );
       IMAGE_SYM_CLASS_UNDEFINED_STATIC:(
       );
       IMAGE_SYM_CLASS_ENUM_TAG:(
       );
       IMAGE_SYM_CLASS_MEMBER_OF_ENUM:(
       );
       IMAGE_SYM_CLASS_REGISTER_PARAM:(
       );
       IMAGE_SYM_CLASS_BIT_FIELD:(
       );
       IMAGE_SYM_CLASS_BLOCK:(
       );
       IMAGE_SYM_CLASS_FUNCTION:(
        BFAndEFSymbols:packed record
         TagIndex:TPACCUInt32;
         LineNumberOrSize:TPACCUInt16;
         TotalSize:TPACCUInt32;
         PointerToNextFunction:TPACCUInt32;
         Dimension:array[0..2] of TPACCUInt16;
         TvIndex:TPACCUInt16;
        end;
       );
       IMAGE_SYM_CLASS_END_OF_STRUCT:(
       );
       IMAGE_SYM_CLASS_FILE:(
        File_:packed record
         Name:array[0..SizeOf(TCOFFSymbol)-1] of AnsiChar;
        end;
       );
       IMAGE_SYM_CLASS_SECTION:(
        Section:packed record
         Length:TPACCUInt32;
         NumberOfRelocations:TPACCUInt16;
         NumberOfLineNumbers:TPACCUInt16;
         CheckSum:TPACCUInt32;
         Number:TPACCUInt16;
         Selection:TPACCUInt8;
        end;
       );
       IMAGE_SYM_CLASS_WEAK_EXTERNAL:(
        WeakExternals:packed record
         TagIndex:TPACCUInt32;
         Characteristics:TPACCUInt32;
        end;
       );
       IMAGE_SYM_CLASS_CLR_TOKEN:(
       );
     end;

     PResourceHeader=^TResourceHeader;
     TResourceHeader=packed record
      DataSize:TPACCUInt32;
      HeaderSize:TPACCUInt32;
      Type_:TPACCUInt32;
      Name:TPACCUInt32;
      DataVersion:TPACCUInt32;
      MemoryFlags:TPACCUInt16;
      LanguageID:TPACCUInt16;
      Version:TPACCUInt32;
      Characteristics:TPACCUInt32;
     end;

     PStrippedResourceHeader=^TStrippedResourceHeader;
     TStrippedResourceHeader=packed record
      DataSize:TPACCUInt32;
      HeaderSize:TPACCUInt32;
     end;

     TResourceNode=class;

     PResourceNodeItem=^TResourceNodeItem;
     TResourceNodeItem=class
      private
       fLeaf:boolean;
       fIsIntegerID:boolean;
       fID:TPUCUUTF16String;
       fObject:TObject;
       fCodePage:TPACCUInt32;
       function GetStream:TMemoryStream;
       procedure SetStream(const AStream:TMemoryStream);
       function GetNode:TResourceNode;
       procedure SetNode(const ANode:TResourceNode);
      protected
       DataEntryOffset:TPACCInt64;
       NameOffset:TPACCInt64;
       DataRVA:TPACCInt64;
       function CheckIntegerID:boolean;
      public
       constructor Create; reintroduce;
       destructor Destroy; override;
      published
       property Leaf:boolean read fLeaf write fLeaf;
       property IsIntegerID:boolean read fIsIntegerID write fIsIntegerID;
       property ID:TPUCUUTF16String read fID write fID;
       property Stream:TMemoryStream read GetStream write SetStream;
       property Node:TResourceNode read GetNode write SetNode;
       property CodePage:TPACCUInt32 read fCodePage write fCodePage;
     end;

     TResourceNodeItems=array of TResourceNodeItem;

     TResourceNode=class(TList)
      private
       function GetItem(const AIndex:TPACCInt):TResourceNodeItem;
       procedure SetItem(const AIndex:TPACCInt;const AItem:TResourceNodeItem);
      protected
       Offset:TPACCInt64;
      public
       constructor Create(const AType,AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       constructor CreateName(const AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       constructor CreateLang(const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       destructor Destroy; override;
       procedure Add(const AType,AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       procedure AddName(const AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       procedure AddLang(const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32); reintroduce; overload;
       property Items[const AIndex:TPACCInt]:TResourceNodeItem read GetItem write SetItem; default;
     end;

var NullBytes:array[0..65535] of TPACCUInt8;

constructor TResourceNodeItem.Create;
begin
 inherited Create;
 fLeaf:=false;
 fIsIntegerID:=false;
 fID:='';
 fObject:=nil;
 fCodePage:=0;
end;

destructor TResourceNodeItem.Destroy;
begin
 if assigned(fObject) and (fObject is TResourceNode) then begin
  FreeAndNil(fObject);
 end;
 inherited Destroy;
end;

function TResourceNodeItem.GetStream:TMemoryStream;
begin
 result:=TMemoryStream(fObject);
end;

procedure TResourceNodeItem.SetStream(const AStream:TMemoryStream);
begin
 fObject:=AStream;
end;

function TResourceNodeItem.GetNode:TResourceNode;
begin
 result:=TResourceNode(fObject);
end;

procedure TResourceNodeItem.SetNode(const ANode:TResourceNode);
begin
 fObject:=ANode;
end;

function TResourceNodeItem.CheckIntegerID:boolean;
var Index:TPACCInt32;
begin
 for Index:=1 to length(fID) do begin
  if (fID[Index]<'0') or (fID[Index]>'9') then begin
   result:=false;
   exit;
  end;
 end;
 try
  result:=TPUCUUTF16String(IntToStr(StrToIntDef(String(fID),-1)))=fID;
 except
  result:=false;
 end;
end;

constructor TResourceNode.Create(const AType,AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Item:TResourceNodeItem;
begin
 inherited Create;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=AType;
 Item.IsIntegerID:=Item.CheckIntegerID;
 Item.Leaf:=false;
 Item.Node:=TResourceNode.CreateName(AName,ALang,AStream,ACodePage);
end;

constructor TResourceNode.CreateName(const AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Item:TResourceNodeItem;
begin
 inherited Create;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=AName;
 Item.IsIntegerID:=Item.CheckIntegerID;
 Item.Leaf:=false;
 Item.Node:=TResourceNode.CreateLang(ALang,AStream,ACodePage);
end;

constructor TResourceNode.CreateLang(const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Item:TResourceNodeItem;
begin
 inherited Create;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=TPUCUUTF16String(IntToStr(ALang));
 Item.IsIntegerID:=true;
 Item.Leaf:=true;
 Item.Stream:=AStream;
 Item.CodePage:=ACodePage;
end;          

destructor TResourceNode.Destroy;
var Index:TPACCInt32;
begin
 for Index:=Count-1 downto 0 do begin
  Items[Index].Free;
  Items[Index]:=nil;
 end;
 inherited Destroy;
end;

function TResourceNode.GetItem(const AIndex:TPACCInt):TResourceNodeItem;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TResourceNode.SetItem(const AIndex:TPACCInt;const AItem:TResourceNodeItem);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

procedure TResourceNode.Add(const AType,AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Index:TPACCInt32;
    Item:TResourceNodeItem;
begin
 for Index:=0 to Count-1 do begin
  Item:=Items[Index];
  if Item.ID=AType then begin
   exit;
  end;
 end;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=AType;
 Item.IsIntegerID:=Item.CheckIntegerID;
 Item.Leaf:=false;
 Item.Node:=TResourceNode.CreateName(AName,ALang,AStream,ACodePage);
end;

procedure TResourceNode.AddName(const AName:TPUCUUTF16String;const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Index:TPACCInt32;
    Item:TResourceNodeItem;
begin
 for Index:=0 to Count-1 do begin
  Item:=Items[Index];
  if Item.ID=AName then begin
   exit;
  end;
 end;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=AName;
 Item.IsIntegerID:=Item.CheckIntegerID;
 Item.Leaf:=false;
 Item.Node:=TResourceNode.CreateLang(ALang,AStream,ACodePage);
end;

procedure TResourceNode.AddLang(const ALang:TPACCInt32;const AStream:TMemoryStream;const ACodePage:TPACCUInt32);
var Index:TPACCInt32;
    Item:TResourceNodeItem;
    Lang:TPUCUUTF16String;
begin
 Lang:=TPUCUUTF16String(IntToStr(ALang));
 for Index:=0 to Count-1 do begin
  Item:=Items[Index];
  if Item.ID=Lang then begin
   exit;
  end;
 end;
 Item:=TResourceNodeItem.Create;
 inherited Add(Item);
 Item.ID:=Lang;
 Item.IsIntegerID:=true;
 Item.Leaf:=true;
 Item.Stream:=AStream;
 Item.CodePage:=ACodePage;
end;

constructor TPACCLinker_COFF_PE_Section.Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const AVirtualAddress:TPACCUInt64;const ACharacteristics:TPACCUInt32);
var Index:TPACCInt32;
begin
 inherited Create;

 fLinker:=ALinker;

 Index:=pos('$',AName);
 if Index>0 then begin
  fName:=copy(AName,1,Index-1);
  fOrdering:=copy(AName,Index+1,length(AName)-Index);
 end else begin
  fName:=AName;
  fOrdering:='';
 end;

 fStream:=TMemoryStream.Create;

 fCharacteristics:=ACharacteristics;

 if (fCharacteristics and IMAGE_SCN_ALIGN_MASK)<>0 then begin
  fAlignment:=1 shl (((fCharacteristics and IMAGE_SCN_ALIGN_MASK) shr IMAGE_SCN_ALIGN_SHIFT)-1);
 end else begin
  fAlignment:=1;
 end;

 fVirtualAddress:=AVirtualAddress;

 fVirtualSize:=0;

 fRawSize:=0;

 fSymbols:=TPACCLinker_COFF_PE_SymbolList.Create;

 fActive:=true;

 Relocations:=nil;
 CountRelocations:=0;

end;

destructor TPACCLinker_COFF_PE_Section.Destroy;
begin
 Relocations:=nil;
 fSymbols.Free;
 fStream.Free;
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_Section.NewRelocation:PPACCLinker_COFF_PE_Relocation;
var Index:TPACCInt32;
begin
 Index:=CountRelocations;
 inc(CountRelocations);
 if length(Relocations)<CountRelocations then begin
  SetLength(Relocations,CountRelocations*2);
 end;
 result:=@Relocations[Index];
end;

constructor TPACCLinker_COFF_PE_SectionList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PE_SectionList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_SectionList.GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Section;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_COFF_PE_SectionList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Section);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_COFF_PE_Symbol.Create(const ALinker:TPACCLinker_COFF_PE;const AName:TPACCRawByteString;const ASection:TPACCLinker_COFF_PE_Section;const AValue:TPACCInt64;const AType,AClass:TPACCInt32;const ASymbolKind:TPACCLinker_COFF_PE_SymbolKind);
begin
 inherited Create;

 fLinker:=ALinker;

 fName:=AName;

 fSection:=ASection;

 fValue:=AValue;

 fType:=AType;

 fClass:=AClass;

 fSymbolKind:=ASymbolKind;

 fAlias:=nil;
 
 fSubSymbols:=TPACCLinker_COFF_PE_SymbolList.Create;

 fAuxData:=nil;

 fActive:=true;

end;

destructor TPACCLinker_COFF_PE_Symbol.Destroy;
begin

 fAuxData.Free;

 while fSubSymbols.Count>0 do begin
  fSubSymbols[fSubSymbols.Count-1].Free;
  fSubSymbols.Delete(fSubSymbols.Count-1);
 end;
 fSubSymbols.Free;

 inherited Destroy;
end;

constructor TPACCLinker_COFF_PE_SymbolList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PE_SymbolList.Destroy;
begin
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_SymbolList.GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Symbol;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_COFF_PE_SymbolList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Symbol);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_COFF_PE_Resource.Create(const ALinker:TPACCLinker_COFF_PE);
begin
 inherited Create;

 fLinker:=ALInker;

 fType_:='';

 fName:='';

 fDataVersion:=0;

 fMemoryFlags:=0;

 fLanguageID:=0;

 fVersion:=0;

 fCharacteristics:=0;

 fCodePage:=0;

 fStream:=TMemoryStream.Create;

end;

destructor TPACCLinker_COFF_PE_Resource.Destroy;
begin
 fType_:='';
 fName:='';
 fStream.Free;
 inherited Destroy;
end;

procedure TPACCLinker_COFF_PE_Resource.UpdateCodePage;
begin
 fCodePage:=LCIDToCodePageLookUpTable[fLanguageID];
end;

constructor TPACCLinker_COFF_PE_ResourceList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PE_ResourceList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_ResourceList.GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Resource;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_COFF_PE_ResourceList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Resource);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_COFF_PE_ImportList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PE_ImportList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_ImportList.GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Import;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_COFF_PE_ImportList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Import);
begin
 inherited Items[AIndex]:=pointer(AItem);
end;

constructor TPACCLinker_COFF_PE_ExportList.Create;
begin
 inherited Create;
end;

destructor TPACCLinker_COFF_PE_ExportList.Destroy;
begin
 while Count>0 do begin
  Items[Count-1].Free;
  Delete(Count-1);
 end;
 inherited Destroy;
end;

function TPACCLinker_COFF_PE_ExportList.GetItem(const AIndex:TPACCInt):TPACCLinker_COFF_PE_Export;
begin
 result:=pointer(inherited Items[AIndex]);
end;

procedure TPACCLinker_COFF_PE_ExportList.SetItem(const AIndex:TPACCInt;const AItem:TPACCLinker_COFF_PE_Export);
begin
 inherited Items[AIndex]:=pointer(AItem);
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

 fSections:=TPACCLinker_COFF_PE_SectionList.Create;

 fSymbols:=TPACCLinker_COFF_PE_SymbolList.Create;

 fResources:=TPACCLinker_COFF_PE_ResourceList.Create;

 fImports:=TPACCLinker_COFF_PE_ImportList.Create;
 fImportSymbolNameHashMap:=TPACCRawByteStringHashMap.Create;

 fExports:=TPACCLinker_COFF_PE_ExportList.Create;
 fExportSymbolNameHashMap:=TPACCRawByteStringHashMap.Create;

 fImageBase:=$400000;

end;

destructor TPACCLinker_COFF_PE.Destroy;
begin

 fResources.Free;

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

 fImports.Free;
 fImportSymbolNameHashMap.Free;

 fExports.Free;
 fExportSymbolNameHashMap.Free;

 inherited Destroy;
end;

procedure TPACCLinker_COFF_PE.AddImport(const ASymbolName,AImportLibraryName,AImportName:TPUCUUTF8String);
var Import_:TPACCLinker_COFF_PE_Import;
begin
 if assigned(fImportSymbolNameHashMap[ASymbolName]) then begin
  TPACCInstance(Instance).AddWarning('Duplicate import symbol name "'+ASymbolName+'"',nil);
 end else begin
  Import_:=TPACCLinker_COFF_PE_Import.Create;
  fImports.Add(Import_);
  Import_.Used:=false;
  Import_.SymbolName:=ASymbolName;
  Import_.ImportLibraryName:=AImportLibraryName;
  Import_.ImportName:=AImportName;
  fImportSymbolNameHashMap[ASymbolName]:=Import_;
 end;
end;

procedure TPACCLinker_COFF_PE.AddExport(const ASymbolName,AExportName:TPUCUUTF8String);
var Export_:TPACCLinker_COFF_PE_Export;
begin
 if assigned( fExportSymbolNameHashMap[ASymbolName]) then begin
  TPACCInstance(Instance).AddWarning('Duplicate export symbol name "'+ASymbolName+'"',nil);
 end else begin
  Export_:=TPACCLinker_COFF_PE_Export.Create;
  fExports.Add(Export_);
  Export_.Used:=false;
  Export_.SymbolName:=ASymbolName;
  Export_.ExportName:=AExportName;
  fExportSymbolNameHashMap[ASymbolName]:=Export_;
 end;
end;

procedure TPACCLinker_COFF_PE.AddObject(const AObjectStream:TStream;const AObjectFileName:TPUCUUTF8String='');
var SectionIndex,RelocationIndex,NumberOfRelocations,SymbolIndex,SectionStartIndex,SymbolStartIndex,Index:TPACCInt32;
    RelocationOffset:TPACCUInt32;
    COFFFileHeader:TCOFFFileHeader;
    LocalSections:TPACCLinker_COFF_PE_SectionList;
    COFFSectionHeaders:TCOFFSectionHeaders;
    COFFSectionHeader:PCOFFSectionHeader;
    Section:TPACCLinker_COFF_PE_Section;
    OldSize,Offset:TPACCInt64;
    COFFRelocations:TCOFFRelocations;
    COFFRelocation:PCOFFRelocation;
    Relocation:PPACCLinker_COFF_PE_Relocation;
    COFFSymbols:TCOFFSymbols;
    COFFSymbol:PCOFFSymbol;
    Symbol:TPACCLinker_COFF_PE_Symbol;
    Name:TPACCRawByteString;
    c:ansichar;
    SymbolKind:TPACCLinker_COFF_PE_SymbolKind;
    SymbolRemap:array of TPACCUInt32;
begin

 COFFSectionHeaders:=nil;

 SymbolRemap:=nil;

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

  SectionStartIndex:=Sections.Count;
  SymbolStartIndex:=Symbols.Count;

  LocalSections:=TPACCLinker_COFF_PE_SectionList.Create;
  try

   // Load sections
   for SectionIndex:=0 to COFFFileHeader.NumberOfSections-1 do begin
    COFFSectionHeader:=@COFFSectionHeaders[SectionIndex];
    if (COFFSectionHeader^.VirtualSize>0) or (COFFSectionHeader^.SizeOfRawData>0) then begin
     Section:=TPACCLinker_COFF_PE_Section.Create(self,COFFSectionHeader^.Name,COFFSectionHeader^.VirtualAddress,COFFSectionHeader^.Characteristics);
     Sections.Add(Section);
     LocalSections.Add(Section);
     if (COFFSectionHeader^.Characteristics and IMAGE_SCN_ALIGN_MASK)<>0 then begin
      Section.Alignment:=1 shl (((COFFSectionHeader^.Characteristics and IMAGE_SCN_ALIGN_MASK) shr IMAGE_SCN_ALIGN_SHIFT)-1);
     end;
     if (COFFSectionHeader^.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA)=0 then begin
      if COFFSectionHeader^.SizeOfRawData>0 then begin
       if (COFFSectionHeader^.VirtualSize>0) and
          (COFFSectionHeader^.SizeOfRawData>COFFSectionHeader^.VirtualSize) then begin
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
        Section.CountRelocations:=NumberOfRelocations;
        for RelocationIndex:=0 to Section.CountRelocations-1 do begin
         COFFRelocation:=@COFFRelocations[RelocationIndex];
         Relocation:=@Section.Relocations[RelocationIndex];
         Relocation^.VirtualAddress:=COFFRelocation^.VirtualAddress+Section.VirtualAddress;
         Relocation^.Symbol:=COFFRelocation^.Symbol;
         Relocation^.RelocationType:=COFFRelocation^.RelocationType;
        end;
       finally
        COFFRelocations:=nil;
       end;
      end;
     end;
     Section.VirtualAddress:=0;
    end else begin
     LocalSections.Add(nil);
    end;
   end;

   // Allocate symbol index remap array
   SetLength(SymbolRemap,COFFFileHeader.NumberOfSymbols);
   for SymbolIndex:=0 to length(SymbolRemap)-1 do begin
    SymbolRemap[SymbolIndex]:=0;
   end;

   // Load symbols
   if (COFFFileHeader.PointerToSymbolTable>0) and (COFFFileHeader.NumberOfSymbols>0) then begin
    COFFSymbols:=nil;
    try
     SetLength(COFFSymbols,COFFFileHeader.NumberOfSymbols);
     if AObjectStream.Seek(COFFFileHeader.PointerToSymbolTable,soBeginning)<>COFFFileHeader.PointerToSymbolTable then begin
      TPACCInstance(Instance).AddError('Stream seek error',nil,true);
     end;
     AObjectStream.ReadBuffer(COFFSymbols[0],COFFFileHeader.NumberOfSymbols*SizeOf(TCOFFSymbol));
     SymbolIndex:=0;
     while TPACCUInt32(SymbolIndex)<COFFFileHeader.NumberOfSymbols do begin
      COFFSymbol:=@COFFSymbols[SymbolIndex];
      if COFFSymbol^.Name.Zero=0 then begin
       Offset:=COFFFileHeader.PointerToSymbolTable+(COFFFileHeader.NumberOfSymbols*SizeOf(TCOFFSymbol))+COFFSymbol^.Name.PointerToString;
       if AObjectStream.Seek(Offset,soBeginning)<>Offset then begin
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
      Section:=nil;
      case COFFSymbol^.Section of
       IMAGE_SYM_UNDEFINED:begin
        SymbolKind:=plcpskUndefined;
       end;
       IMAGE_SYM_ABSOLUTE:begin
        SymbolKind:=plcpskAbsolute;
       end;
       IMAGE_SYM_DEBUG:begin
        SymbolKind:=plcpskDebug;
       end;
       else begin
        if (COFFSymbol^.Section>0) and (COFFSymbol^.Section<=LocalSections.Count) then begin
         Section:=LocalSections[COFFSymbol^.Section-1];
         SymbolKind:=plcpskNormal;
        end else begin
         SymbolKind:=plcpskUndefined;
        end;
       end;
      end;
      Symbol:=TPACCLinker_COFF_PE_Symbol.Create(self,Name,Section,COFFSymbol^.Value,COFFSymbol^.SymbolType,COFFSymbol^.SymbolClass,SymbolKind);
      SymbolRemap[SymbolIndex]:=Symbols.Add(Symbol);
      inc(SymbolIndex);
      if assigned(Section) then begin
       Section.Symbols.Add(Symbol);
      end;
      if COFFSymbol^.Aux>0 then begin
       if not assigned(Symbol.fAuxData) then begin
        Symbol.fAuxData:=TMemoryStream.Create;
       end;
       Symbol.fAuxData.WriteBuffer(COFFSymbols[SymbolIndex],COFFSymbol^.Aux*SizeOf(TCOFFSymbol));
       inc(SymbolIndex,COFFSymbol^.Aux);
       case Symbol.Class_ of
        IMAGE_SYM_CLASS_EXTERNAL:begin
         inc(PImageAuxSymbol(Symbol.fAuxData.Memory)^.FunctionDefinition.PointerToNextFunction,SymbolStartIndex);
        end;
        IMAGE_SYM_CLASS_STATIC:begin
         if PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number>0 then begin
          inc(PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number,SectionStartIndex);
         end;
        end;
        IMAGE_SYM_CLASS_FUNCTION:begin
         inc(PImageAuxSymbol(Symbol.fAuxData.Memory)^.BFAndEFSymbols.PointerToNextFunction,SymbolStartIndex);
        end;
        IMAGE_SYM_CLASS_SECTION:begin
         if PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number>0 then begin
          inc(PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number,SectionStartIndex);
         end;
        end;
       end;
      end;
     end;
    finally
     COFFSymbols:=nil;
    end;
   end;

   // Correct symbol indices at section relocations
   for SectionIndex:=SectionStartIndex to Sections.Count-1 do begin
    Section:=Sections[SectionIndex];
    for RelocationIndex:=0 to Section.CountRelocations-1 do begin
     Relocation:=@Section.Relocations[RelocationIndex];
     Assert(Relocation^.Symbol<TPACCUInt32(length(SymbolRemap)));
     Relocation^.Symbol:=SymbolRemap[Relocation^.Symbol];
    end;
   end;

  finally
   LocalSections.Free;
  end;

 finally
  COFFSectionHeaders:=nil;
  SymbolRemap:=nil;
 end;

end;

procedure TPACCLinker_COFF_PE.AddResources(const AResourcesStream:TStream;const AResourcesFileName:TPUCUUTF8String='');
var Is16Bit:boolean;
 procedure AddResource(const HeaderStream,DataStream:TStream);
  function GetString:TPUCUUTF16String;
  var Value:TPACCUInt16;
  begin
   HeaderStream.ReadBuffer(Value,SizeOf(TPACCUInt16));
   if Value=$ffff then begin
    HeaderStream.ReadBuffer(Value,SizeOf(TPACCUInt16));
    result:=TPUCUUTF16String(IntToStr(Value));
   end else begin
    result:=TPUCUUTF16Char(Value);
    while HeaderStream.Position<HeaderStream.Size do begin
     HeaderStream.ReadBuffer(Value,SizeOf(TPACCUInt16));
     if Value=0 then begin
      break;
     end else begin
      result:=result+TPUCUUTF16Char(Value);
     end;
    end;
   end;
  end;
 var DataSize:TPACCUInt32;
     HeaderSize:TPACCUInt32;
     Type_:TPUCUUTF16String;
     Name:TPUCUUTF16String;
     DataVersion:TPACCUInt32;
     MemoryFlags:TPACCUInt16;
     LanguageID:TPACCUInt16;
     Version:TPACCUInt32;
     Characteristics:TPACCUInt32;
     Resource:TPACCLinker_COFF_PE_Resource;
 begin
  HeaderStream.ReadBuffer(DataSize,SizeOf(TPACCUInt32));
  HeaderStream.ReadBuffer(HeaderSize,SizeOf(TPACCUInt32));
  Type_:=GetString;
  Name:=GetString;
  HeaderStream.Seek((HeaderStream.Position+3) and not TPACCInt64(3),soBeginning);
  HeaderStream.ReadBuffer(DataVersion,SizeOf(TPACCUInt32));
  HeaderStream.ReadBuffer(MemoryFlags,SizeOf(TPACCUInt16));
  HeaderStream.ReadBuffer(LanguageID,SizeOf(TPACCUInt16));
  HeaderStream.ReadBuffer(Version,SizeOf(TPACCUInt32));
  HeaderStream.ReadBuffer(Characteristics,SizeOf(TPACCUInt32));
  if (DataSize<>0) or (Name<>'0') then begin
   Resource:=TPACCLinker_COFF_PE_Resource.Create(self);
   Resources.Add(Resource);
   Resource.Type_:=Type_;
   Resource.Name:=Name;
   Resource.DataVersion:=DataVersion;
   Resource.MemoryFlags:=MemoryFlags;
   Resource.LanguageID:=LanguageID;
   Resource.Version:=Version;
   Resource.Characteristics:=Characteristics;
   Resource.UpdateCodePage;
   DataStream.Seek(0,soBeginning);
   Resource.Stream.LoadFromStream(DataStream);
   Resource.Stream.Seek(0,soBeginning);
  end else begin
   Is16Bit:=false;
  end;
 end;
var StrippedResourceHeader:TStrippedResourceHeader;
    HeaderStream,ResourceStream:TMemoryStream;
    LastPosition:TPACCInt64;
begin
 Is16Bit:=true;
 while (AResourcesStream.Position+SizeOf(TStrippedResourceHeader))<AResourcesStream.Size do begin
  LastPosition:=AResourcesStream.Position;
  AResourcesStream.ReadBuffer(StrippedResourceHeader,SizeOf(TStrippedResourceHeader));
  HeaderStream:=TMemoryStream.Create;
  try
   HeaderStream.WriteBuffer(StrippedResourceHeader,SizeOf(StrippedResourceHeader));
   if SizeOf(TStrippedResourceHeader)<StrippedResourceHeader.HeaderSize then begin
    if HeaderStream.CopyFrom(AResourcesStream,StrippedResourceHeader.HeaderSize-SizeOf(TStrippedResourceHeader))<>(StrippedResourceHeader.HeaderSize-SizeOf(TStrippedResourceHeader)) then begin
     raise EReadError.Create('Stream read error');
    end;
   end;
   ResourceStream:=TMemoryStream.Create;
   try
    if StrippedResourceHeader.DataSize>0 then begin
     ResourceStream.CopyFrom(AResourcesStream,StrippedResourceHeader.DataSize);
    end;
    HeaderStream.Seek(0,soBeginning);
    ResourceStream.Seek(0,soBeginning);
    AddResource(HeaderStream,ResourceStream);
   finally
    ResourceStream.Free;
   end;
  finally
   HeaderStream.Free;
  end;
  AResourcesStream.Seek(LastPosition+(((StrippedResourceHeader.DataSize+StrippedResourceHeader.HeaderSize)+3) and not TPACCInt32(3)),soBeginning);
 end;
 if Is16Bit then begin
 end;
end;

function CompareSections(a,b:pointer):TPACCInt32;
begin
 result:=TPACCLinker_COFF_PE_Section(a).Order-TPACCLinker_COFF_PE_Section(b).Order;
 if (result=0) and (TPACCLinker_COFF_PE_Section(a).Name=TPACCLinker_COFF_PE_Section(b).Name) then begin
  result:=CompareStr(TPACCLinker_COFF_PE_Section(a).Ordering,TPACCLinker_COFF_PE_Section(b).Ordering);
 end;
end;

procedure TPACCLinker_COFF_PE.Link(const AOutputStream:TStream;const AOutputFileName:TPUCUUTF8String='');
type PRelocationNode=^TRelocationNode;
     TRelocationNode=packed record
      Next:PRelocationNode;
      Previous:PRelocationNode;
      VirtualAddress:TPACCUInt32;
      RelocationType:TPACCUInt32;
     end;
     PRelocations=^TRelocations;
     TRelocations=packed record
      RootNode,LastNode:PRelocationNode;
     end;
     PPECOFFDirectoryEntry=^TPECOFFDirectoryEntry;
     TPECOFFDirectoryEntry=record
      Section:TPACCLinker_COFF_PE_Section;
      Offset:TPACCUInt32;
      Size:TPACCUInt32;
     end;
     PPECOFFDirectoryEntries=^TPECOFFDirectoryEntries;
     TPECOFFDirectoryEntries=array[0..IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1] of TPECOFFDirectoryEntry;
var Relocations:TRelocations;
    LastVirtualAddress:TPACCUInt64;
    PECOFFDirectoryEntries:PPECOFFDirectoryEntries;
    Is64Bit:boolean;
    ExternalAvailableSymbolHashMap:TPACCRawByteStringHashMap;
 procedure RelocationsInit(out Instance:TRelocations);
 begin
  FillChar(Instance,SizeOf(TRelocations),#0);
 end;
 procedure RelocationsDone(var Instance:TRelocations);
 var CurrentNode,NextNode:PRelocationNode;
 begin
  CurrentNode:=Instance.RootNode;
  Instance.RootNode:=nil;
  Instance.LastNode:=nil;
  while assigned(CurrentNode) do begin
   NextNode:=CurrentNode^.Next;
   FreeMem(CurrentNode);
   CurrentNode:=NextNode;
  end;
 end;
 procedure RelocationsAdd(var Instance:TRelocations;const VirtualAddress,RelocationType:TPACCUInt32);
 var NewNode:PRelocationNode;
 begin
  GetMem(NewNode,SizeOf(TRelocationNode));
  FillChar(NewNode^,SizeOf(TRelocationNode),#0);
  NewNode^.VirtualAddress:=VirtualAddress;
  NewNode^.RelocationType:=RelocationType;
  if assigned(Instance.LastNode) then begin
   Instance.LastNode^.Next:=NewNode;
   NewNode^.Previous:=Instance.LastNode;
  end else begin
   Instance.RootNode:=NewNode;
  end;
  Instance.LastNode:=NewNode;
 end;
 procedure RelocationsSort(var Instance:TRelocations);
 var PartA,PartB,Node:PRelocationNode;
     InSize,PartASize,PartBSize,Merges:TPACCInt32;
 begin
  if assigned(Instance.RootNode) then begin
   InSize:=1;
   while true do begin
    PartA:=Instance.RootNode;
    Instance.RootNode:=nil;
    Instance.LastNode:=nil;
    Merges:=0;
    while assigned(PartA) do begin
     inc(Merges);
     PartB:=PartA;
     PartASize:=0;
     while PartASize<InSize do begin
      inc(PartASize);
      PartB:=PartB^.Next;
      if not assigned(PartB) then begin
       break;
      end;
     end;
     PartBSize:=InSize;
     while (PartASize>0) or ((PartBSize>0) and assigned(PartB)) do begin
      if PartASize=0 then begin
       Node:=PartB;
       PartB:=PartB^.Next;
       dec(PartBSize);
      end else if (PartBSize=0) or not assigned(PartB) then begin
       Node:=PartA;
       PartA:=PartA^.Next;
       dec(PartASize);
      end else if PartA^.VirtualAddress<=PartB^.VirtualAddress then begin
       Node:=PartA;
       PartA:=PartA^.Next;
       dec(PartASize);
      end else begin
       Node:=PartB;
       PartB:=PartB^.Next;
       dec(PartBSize);
      end;
      if assigned(Instance.LastNode) then begin
       Instance.LastNode^.Next:=Node;
      end else begin
       Instance.RootNode:=Node;
      end;
      Node^.Previous:=Instance.LastNode;
      Instance.LastNode:=Node;
     end;
     PartA:=PartB;
    end;
    Instance.LastNode^.Next:=nil;
    if Merges<=1 then begin
     break;
    end;
    inc(InSize,InSize);
   end;
  end;
 end;
 function RelocationsSize(var Instance:TRelocations):TPACCUInt32;
 var CurrentNode,OldNode:PRelocationNode;
 begin
  RelocationsSort(Instance);
  result:=0;
  CurrentNode:=Instance.RootNode;
  OldNode:=CurrentNode;
  while assigned(CurrentNode) do begin
   if (CurrentNode=OldNode) or ((CurrentNode^.VirtualAddress-OldNode^.VirtualAddress)>=$1000) then begin
    inc(result,sizeof(TImageBaseRelocation));
   end;
   inc(result,sizeof(TPACCUInt16));
   OldNode:=CurrentNode;
   CurrentNode:=CurrentNode^.Next;
  end;
  inc(result,sizeof(TImageBaseRelocation));
 end;
 procedure RelocationsBuild(var Instance:TRelocations;NewBase:pointer;VirtualAddress:TPACCUInt32);
 var CurrentNode,OldNode:PRelocationNode;
     CurrentPointer:pchar;
     BaseRelocation:PImageBaseRelocation;
 begin
  RelocationsSort(Instance);
  CurrentPointer:=NewBase;
  BaseRelocation:=pointer(CurrentPointer);
  CurrentNode:=Instance.RootNode;
  OldNode:=CurrentNode;
  while assigned(CurrentNode) do begin
   if (CurrentNode=OldNode) or ((CurrentNode^.VirtualAddress-OldNode^.VirtualAddress)>=$1000) then begin
    BaseRelocation:=pointer(CurrentPointer);
    inc(CurrentPointer,sizeof(TImageBaseRelocation));
    BaseRelocation^.VirtualAddress:=CurrentNode^.VirtualAddress;
    BaseRelocation^.SizeOfBlock:=sizeof(TImageBaseRelocation);
   end;
   PPACCUInt16(CurrentPointer)^:=(CurrentNode^.RelocationType shl 12) or ((CurrentNode^.VirtualAddress-BaseRelocation^.VirtualAddress) and $fff);
   inc(CurrentPointer,sizeof(TPACCUInt16));
   inc(BaseRelocation^.SizeOfBlock,sizeof(TPACCUInt16));
   OldNode:=CurrentNode;
   CurrentNode:=CurrentNode^.Next;
  end;
  BaseRelocation:=pointer(CurrentPointer);
  inc(CurrentPointer,sizeof(TImageBaseRelocation));
  BaseRelocation^.VirtualAddress:=0;
  BaseRelocation^.SizeOfBlock:=0;
 end;
 procedure RelocationsDump(var Instance:TRelocations);
 var CurrentNode:PRelocationNode;
 begin
  CurrentNode:=Instance.RootNode;
  while assigned(CurrentNode) do begin
   writeln(CurrentNode^.VirtualAddress);
   CurrentNode:=CurrentNode^.Next;
  end;
 end;
 function SectionSizeAlign(Size:TPACCInt64):TPACCInt64;
 begin
  result:=Size;
  if (result and (PECOFFSectionAlignment-1))<>0 then begin
   result:=(result+(PECOFFSectionAlignment-1)) and not (PECOFFSectionAlignment-1);
  end;
 end;
 function FileSizeAlign(Size:TPACCInt64):TPACCInt64;
 begin
  result:=Size;
  if (result and (PECOFFFileAlignment-1))<>0 then begin
   result:=(result+(PECOFFFileAlignment-1)) and not (PECOFFFileAlignment-1);
  end;
 end;
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
   IMAGE_FILE_MACHINE_I386:begin
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
   IMAGE_FILE_MACHINE_AMD64:begin
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
 procedure DoAlign;
 var Position,FillUpCount,ToDoCount:TPACCInt64;
 begin
  Position:=AOutputStream.Position;
  if (Position and (PECOFFFileAlignment-1))<>0 then begin
   FillUpCount:=((Position+(PECOFFFileAlignment-1)) and not (PECOFFFileAlignment-1))-Position;
   while FillUpCount>0 do begin
    ToDoCount:=Min(FillUpCount,SizeOf(NullBytes));
    AOutputStream.WriteBuffer(NullBytes[0],ToDoCount);
    dec(FillUpCount,ToDoCount);
   end;
  end;
 end;
 procedure ScanImports;
 var SymbolIndex:TPACCInt32;
     Symbol:TPACCLinker_COFF_PE_Symbol;
     Import_:TPACCLinker_COFF_PE_Import;
 begin
  for SymbolIndex:=0 to Symbols.Count-1 do begin
   Symbol:=Symbols[SymbolIndex];
   if Symbol.Active and (Symbol.Class_=IMAGE_SYM_CLASS_EXTERNAL) and
      ((Symbol.SymbolKind=plcpskUndefined) or
       ((Symbol.SymbolKind=plcpskNormal) and not assigned(Symbol.Section))) and
       not assigned(Symbol.Alias) then begin
    if (length(Symbol.Name)>6) and
       (Symbol.Name[1]='_') and
       (Symbol.Name[2]='_') and
       (Symbol.Name[3]='i') and
       (Symbol.Name[4]='m') and
       (Symbol.Name[5]='p') and
       (Symbol.Name[6]='_') then begin
     Import_:=fImportSymbolNameHashMap[copy(Symbol.Name,7,length(Symbol.Name)-6)];
    end else begin
     Import_:=fImportSymbolNameHashMap[Symbol.Name];
    end;
    if assigned(Import_) then begin
     Import_.Used:=true;
    end;
   end;
  end;
 end;
 procedure GenerateImports;
 const ImportThunkX86:array[0..7] of TPACCUInt8=($ff,$25,$00,$00,$00,$00,$8b,$c0);
 type PSectionBytes=^TSectionBytes;
      TSectionBytes=array[0..65535] of TPACCUInt8;
      PImportLibraryImport=^TImportLibraryImport;
      TImportLibraryImport=record
       SymbolName:TPACCRawByteString;
       Name:TPACCRawByteString;
       NameOffset:TPACCUInt64;
       CodeOffset:TPACCUInt64;
       CodePatchOffset:TPACCUInt64;
      end;
      TImportLibraryImports=array of TImportLibraryImport;
      PImportLibrary=^TImportLibrary;
      TImportLibrary=record
       Name:TPACCRawByteString;
       DescriptorOffset:TPACCUInt64;
       NameOffset:TPACCUInt64;
       ThunkOffset:TPACCUInt64;
       Imports:TImportLibraryImports;
       CountImports:longint;
      end;
      TImportLibraries=array of TImportLibrary;
 var ImportIndex,SectionIndex,LibraryIndex,CountLibraries,LibraryImportIndex,PassIndex,
     ImportSectionSymbolIndex,CodeSectionSymbolIndex,FirstThunkSymbolIndex,
     LibraryNameSymbolIndex,LibraryImportSymbolIndex,LibraryImportNameSymbolIndex,
     LibraryImportTrunkCodeSymbolIndex:TPACCInt32;
     Import_:TPACCLinker_COFF_PE_Import;
     OK:boolean;
     Section,CodeSection,ImportSection:TPACCLinker_COFF_PE_Section;
     Relocation:PPACCLinker_COFF_PE_Relocation;
     Libraries:TImportLibraries;
     Library_:PImportLibrary;
     LibraryImport:PImportLibraryImport;
     LibraryStringHashMap:TPACCRawByteStringHashMap;
     Entity:PPACCRawByteStringHashMapEntity;
     ImageImportDescriptor:TImageImportDescriptor;
     ImportSectionSymbol,CodeSectionSymbol,FirstThunkSymbol,
     LibraryNameSymbol,LibraryImportSymbol,LibraryImportNameSymbol,
     LibraryImportTrunkCodeSymbol:TPACCLinker_COFF_PE_Symbol;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
 begin

  OK:=false;
  for ImportIndex:=0 to fImports.Count-1 do begin
   Import_:=fImports[ImportIndex];
   if Import_.Used then begin
    OK:=true;
    break;
   end;
  end;

  if OK then begin

   for SectionIndex:=0 to Sections.Count-1 do begin
    Section:=Sections[SectionIndex];
    if Section.Name='.idata' then begin
     TPACCInstance(Instance).AddError('Section ".idata" already exist',nil,true);
    end else if (Section.Name='.text') and (Section.fOrdering='imports') then begin
     TPACCInstance(Instance).AddError('Section ".text" with ordering "imports" already exist',nil,true);
    end;
   end;

   CodeSection:=TPACCLinker_COFF_PE_Section.Create(self,'.text$imports',0,IMAGE_SCN_CNT_CODE or IMAGE_SCN_MEM_READ or IMAGE_SCN_MEM_EXECUTE or IMAGE_SCN_ALIGN_16BYTES);
   Sections.Add(CodeSection);

   ImportSection:=TPACCLinker_COFF_PE_Section.Create(self,'.idata',0,IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_ALIGN_16BYTES);
   Sections.Add(ImportSection);

   Libraries:=nil;
   CountLibraries:=0;
   try

    LibraryStringHashMap:=TPACCRawByteStringHashMap.Create;
    try

     for ImportIndex:=0 to fImports.Count-1 do begin
      Import_:=fImports[ImportIndex];
      if Import_.Used then begin
       Entity:=LibraryStringHashMap.Get(Import_.ImportLibraryName,false);
       if assigned(Entity) then begin
        LibraryIndex:=TPACCPtrUInt(pointer(Entity.Value));
       end else begin
        LibraryIndex:=CountLibraries;
        inc(CountLibraries);
        if length(Libraries)<CountLibraries then begin
         SetLength(Libraries,CountLibraries*2);
        end;
        LibraryStringHashMap[Import_.ImportLibraryName]:=pointer(TPACCPtrUInt(LibraryIndex));
        Library_:=@Libraries[LibraryIndex];
        Library_^.Name:=Import_.ImportLibraryName;
        Library_^.DescriptorOffset:=0;
        Library_^.NameOffset:=0;
        Library_^.ThunkOffset:=0;
        Library_^.Imports:=nil;
        Library_^.CountImports:=0;
       end;
       Library_:=@Libraries[LibraryIndex];
       LibraryImportIndex:=Library_^.CountImports;
       inc(Library_^.CountImports);
       if length(Library_^.Imports)<Library_^.CountImports then begin
        SetLength(Library_^.Imports,Library_^.CountImports*2);
       end;
       LibraryImport:=@Library_^.Imports[LibraryImportIndex];
       LibraryImport^.SymbolName:=Import_.SymbolName;
       LibraryImport^.Name:=Import_.ImportName;
       LibraryImport^.NameOffset:=0;
       LibraryImport^.CodeOffset:=0;
       LibraryImport^.CodePatchOffset:=0;
      end;
     end;
    finally
     LibraryStringHashMap.Free;
    end;
    SetLength(Libraries,CountLibraries);
    for LibraryIndex:=0 to length(Libraries)-1 do begin
     SetLength(Libraries[LibraryIndex].Imports,Libraries[LibraryIndex].CountImports);
    end;

    for PassIndex:=0 to 1 do begin

     ImportSection.Stream.Seek(0,soBeginning);
     CodeSection.Stream.Seek(0,soBeginning);

     if PassIndex=1 then begin
      ImportSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__import_data_section',ImportSection,0,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      ImportSectionSymbolIndex:=Symbols.Add(ImportSectionSymbol);
      if ImportSectionSymbolIndex>0 then begin
      end;
      ImportSection.Symbols.Add(ImportSectionSymbol);
      CodeSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__import_code_section',CodeSection,0,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      CodeSectionSymbolIndex:=Symbols.Add(CodeSectionSymbol);
      if CodeSectionSymbolIndex>0 then begin
      end;
      CodeSection.Symbols.Add(CodeSectionSymbol);
     end;

     for LibraryIndex:=0 to CountLibraries-1 do begin

      Library_:=@Libraries[LibraryIndex];
      Library_^.DescriptorOffset:=ImportSection.Stream.Position;

      if PassIndex=1 then begin

       begin
        FirstThunkSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__import_library_'+IntToStr(LibraryIndex)+'_thunk',ImportSection,Library_^.ThunkOffset,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
        FirstThunkSymbolIndex:=Symbols.Add(FirstThunkSymbol);
        ImportSection.Symbols.Add(FirstThunkSymbol);
        Relocation:=ImportSection.NewRelocation;
        Relocation^.VirtualAddress:=TPACCPtrUInt(pointer(@PImageImportDescriptor(@PSectionBytes(ImportSection.Stream.Memory)^[ImportSection.Stream.Position])^.FirstThunk))-TPACCPtrUInt(pointer(@PSectionBytes(ImportSection.Stream.Memory)^[0]));
        Relocation^.Symbol:=FirstThunkSymbolIndex;
        case fMachine of
         IMAGE_FILE_MACHINE_I386:begin
          Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
         end;
         else begin
          Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR64NB;
         end;
        end;
       end;

       begin
        LibraryNameSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__import_library_'+IntToStr(LibraryIndex)+'_name',ImportSection,Library_^.NameOffset,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
        LibraryNameSymbolIndex:=Symbols.Add(LibraryNameSymbol);
        ImportSection.Symbols.Add(LibraryNameSymbol);
        Relocation:=ImportSection.NewRelocation;
        Relocation^.VirtualAddress:=TPACCPtrUInt(pointer(@PImageImportDescriptor(@PSectionBytes(ImportSection.Stream.Memory)^[ImportSection.Stream.Position])^.Name))-TPACCPtrUInt(pointer(@PSectionBytes(ImportSection.Stream.Memory)^[0]));
        Relocation^.Symbol:=LibraryNameSymbolIndex;
        case fMachine of
         IMAGE_FILE_MACHINE_I386:begin
          Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
         end;
         else begin
          Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR64NB;
         end;
        end;
       end;

      end;

      FillChar(ImageImportDescriptor,SizeOf(TImageImportDescriptor),#0);
      ImportSection.Stream.WriteBuffer(ImageImportDescriptor,SizeOf(TImageImportDescriptor));

     end;

     FillChar(ImageImportDescriptor,SizeOf(TImageImportDescriptor),#0);
     ImportSection.Stream.WriteBuffer(ImageImportDescriptor,SizeOf(TImageImportDescriptor));

     for LibraryIndex:=0 to CountLibraries-1 do begin

      Library_:=@Libraries[LibraryIndex];
      Library_^.ThunkOffset:=ImportSection.Stream.Position;

      for LibraryImportIndex:=0 to Library_^.CountImports-1 do begin

       LibraryImport:=@Library_^.Imports[LibraryImportIndex];

       if PassIndex=1 then begin

        LibraryImportTrunkCodeSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,LibraryImport^.SymbolName,CodeSection,LibraryImport^.CodeOffset,0,IMAGE_SYM_CLASS_EXTERNAL,plcpskNormal);
        LibraryImportTrunkCodeSymbolIndex:=Symbols.Add(LibraryImportTrunkCodeSymbol);
        CodeSection.Symbols.Add(LibraryImportTrunkCodeSymbol);
        if LibraryImportTrunkCodeSymbolIndex<>0 then begin
        end;

        LibraryImportSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'__imp_'+LibraryImport^.SymbolName,ImportSection,ImportSection.Stream.Position,0,IMAGE_SYM_CLASS_EXTERNAL,plcpskNormal);
        LibraryImportSymbolIndex:=Symbols.Add(LibraryImportSymbol);
        ImportSection.Symbols.Add(LibraryImportSymbol);
        Relocation:=CodeSection.NewRelocation;            
        Relocation^.VirtualAddress:=LibraryImport^.CodePatchOffset;
        Relocation^.Symbol:=LibraryImportSymbolIndex;
        case fMachine of
         IMAGE_FILE_MACHINE_I386:begin
          Relocation^.RelocationType:=IMAGE_REL_I386_DIR32;
         end;
         else begin
          Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32;
         end;
        end;

        LibraryImportNameSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__import_library_'+IntToStr(LibraryIndex)+'_import_'+IntToStr(LibraryImportIndex)+'_name',ImportSection,LibraryImport^.NameOffset,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
        LibraryImportNameSymbolIndex:=Symbols.Add(LibraryImportNameSymbol);
        ImportSection.Symbols.Add(LibraryImportNameSymbol);
        Relocation:=ImportSection.NewRelocation;
        Relocation^.VirtualAddress:=ImportSection.Stream.Position;
        Relocation^.Symbol:=LibraryImportNameSymbolIndex;
        case fMachine of
         IMAGE_FILE_MACHINE_I386:begin
          Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
         end;
         else begin
          Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR64NB;
         end;
        end;

       end;

       case fMachine of
        IMAGE_FILE_MACHINE_I386:begin
         ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt32));
        end;
        else begin
         ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt64));
        end;
       end;

      end;

      case fMachine of
       IMAGE_FILE_MACHINE_I386:begin
        ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt32));
       end;
       else begin
        ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt64));
       end;
      end;

     end;

     for LibraryIndex:=0 to CountLibraries-1 do begin

      Library_:=@Libraries[LibraryIndex];

      Library_^.NameOffset:=ImportSection.Stream.Position;
      ImportSection.Stream.WriteBuffer(Library_^.Name[1],length(Library_^.Name));
      ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt8));

      for LibraryImportIndex:=0 to Library_^.CountImports-1 do begin

       LibraryImport:=@Library_^.Imports[LibraryImportIndex];

       LibraryImport^.NameOffset:=ImportSection.Stream.Position;
       ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt16));
       ImportSection.Stream.WriteBuffer(LibraryImport^.Name[1],length(LibraryImport^.Name));
       ImportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt8));

       LibraryImport^.CodeOffset:=CodeSection.Stream.Position;
       LibraryImport^.CodePatchOffset:=CodeSection.Stream.Position+2;
       CodeSection.Stream.WriteBuffer(ImportThunkX86,SizeOf(ImportThunkX86));

      end;

     end;

    end;

   finally
    Libraries:=nil;
   end;

   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_IMPORT];
   PECOFFDirectoryEntry^.Section:=ImportSection;
   PECOFFDirectoryEntry^.Offset:=0;
   PECOFFDirectoryEntry^.Size:=ImportSection.Stream.Size;

  end;
 end;
 procedure ScanExports;
 var SymbolIndex:TPACCInt32;
     Symbol:TPACCLinker_COFF_PE_Symbol;
     Entity:PPACCRawByteStringHashMapEntity;
     Export_:TPACCLinker_COFF_PE_Export;
 begin
  for SymbolIndex:=0 to Symbols.Count-1 do begin
   Symbol:=Symbols[SymbolIndex];
   if Symbol.Active and (Symbol.Class_=IMAGE_SYM_CLASS_EXTERNAL) and (Symbol.SymbolKind=plcpskNormal) and assigned(Symbol.Section) then begin
    if (length(Symbol.Name)>6) and
       (Symbol.Name[1]='_') and
       (Symbol.Name[2]='_') and
       (Symbol.Name[3]='e') and
       (Symbol.Name[4]='x') and
       (Symbol.Name[5]='p') and
       (Symbol.Name[6]='_') then begin
     Export_:=fExportSymbolNameHashMap[copy(Symbol.Name,7,length(Symbol.Name)-6)];
    end else begin
     Export_:=fExportSymbolNameHashMap[Symbol.Name];
    end;
    if assigned(Export_) then begin
     Export_.Used:=true;
    end;
   end;
  end;
 end;
 procedure GenerateExports;
 var ExportIndex,SectionIndex,PassIndex:TPACCInt32;
     AddressOfName,AddressOfFunctions,AddressOfNames,AddressOfOrdinals,Value:TPACCUInt32;
     Value16:TPACCUInt16;
     Export_:TPACCLinker_COFF_PE_Export;
     Section,ExportSection:TPACCLinker_COFF_PE_Section;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
     ImageExportDirectory:TImageExportDirectory;
     ExportList:TStringList;
     ExportName:TPACCRawByteString;
     ExportSectionSymbol,TempSectionSymbol:TPACCLinker_COFF_PE_Symbol;
     ExportSectionSymbolIndex,TempSectionSymbolIndex:TPACCInt32;
     Relocation:PPACCLinker_COFF_PE_Relocation;
 begin

  ExportList:=TStringList.Create;
  try

   ExportList.NameValueSeparator:='=';
   for ExportIndex:=0 to fExports.Count-1 do begin
    Export_:=fExports[ExportIndex];
    if Export_.Used then begin
     ExportList.Add(Export_.ExportName+ExportList.NameValueSeparator+Export_.SymbolName);
    end;
   end;

   if ExportList.Count>0 then begin

    ExportList.Sort;

    ExportSection:=nil;
    for SectionIndex:=0 to Sections.Count-1 do begin
     Section:=Sections[SectionIndex];
     if Section.Name='.edata' then begin
      ExportSection:=Section;
      break;
     end;
    end;
    if assigned(ExportSection) then begin
     ExportSection.Characteristics:=ExportSection.Characteristics or IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_ALIGN_16BYTES;
     ExportSection.Alignment:=Max(ExportSection.Alignment,16);
    end else begin
     ExportSection:=TPACCLinker_COFF_PE_Section.Create(self,'.edata',0,IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_ALIGN_16BYTES);
     Sections.Add(ExportSection);
    end;

    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXPORT];
    PECOFFDirectoryEntry^.Section:=ExportSection;
    PECOFFDirectoryEntry^.Offset:=ExportSection.Stream.Size;

    AddressOfName:=0;
    AddressOfFunctions:=0;
    AddressOfNames:=0;
    AddressOfOrdinals:=0;

    for PassIndex:=0 to 1 do begin

     ExportSection.Stream.Seek(PECOFFDirectoryEntry^.Offset,soBeginning);

     if PassIndex=1 then begin

      ExportSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section',ExportSection,0,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      ExportSectionSymbolIndex:=Symbols.Add(ExportSectionSymbol);
      if ExportSectionSymbolIndex>0 then begin
      end;
      ExportSection.Symbols.Add(ExportSectionSymbol);

      TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section_addressof_AddressOfName',ExportSection,AddressOfName,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
      ExportSection.Symbols.Add(TempSectionSymbol);
      Relocation:=ExportSection.NewRelocation;
      Relocation^.VirtualAddress:=ExportSection.Stream.Position+TPACCPtrUInt(pointer(@PImageExportDirectory(nil)^.Name));
      Relocation^.Symbol:=TempSectionSymbolIndex;
      case fMachine of
       IMAGE_FILE_MACHINE_I386:begin
        Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
       end;
       else begin
        Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
       end;
      end;

      TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section_addressof_AddressOfFunctions',ExportSection,AddressOfFunctions,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
      ExportSection.Symbols.Add(TempSectionSymbol);
      Relocation:=ExportSection.NewRelocation;
      Relocation^.VirtualAddress:=ExportSection.Stream.Position+TPACCPtrUInt(pointer(@PImageExportDirectory(nil)^.AddressOfFunctions));
      Relocation^.Symbol:=TempSectionSymbolIndex;
      case fMachine of
       IMAGE_FILE_MACHINE_I386:begin
        Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
       end;
       else begin
        Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
       end;
      end;

      TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section_addressof_AddressOfNames',ExportSection,AddressOfNames,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
      ExportSection.Symbols.Add(TempSectionSymbol);
      Relocation:=ExportSection.NewRelocation;
      Relocation^.VirtualAddress:=ExportSection.Stream.Position+TPACCPtrUInt(pointer(@PImageExportDirectory(nil)^.AddressOfNames));
      Relocation^.Symbol:=TempSectionSymbolIndex;
      case fMachine of
       IMAGE_FILE_MACHINE_I386:begin
        Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
       end;
       else begin
        Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
       end;
      end;

      TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section_addressof_AddressOfNameOrdinals',ExportSection,AddressOfOrdinals,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
      TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
      ExportSection.Symbols.Add(TempSectionSymbol);
      Relocation:=ExportSection.NewRelocation;
      Relocation^.VirtualAddress:=ExportSection.Stream.Position+TPACCPtrUInt(pointer(@PImageExportDirectory(nil)^.AddressOfNameOrdinals));
      Relocation^.Symbol:=TempSectionSymbolIndex;
      case fMachine of
       IMAGE_FILE_MACHINE_I386:begin
        Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
       end;
       else begin
        Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
       end;
      end;

     end;

     FillChar(ImageExportDirectory,SizeOf(TImageExportDirectory),#0);
     ImageExportDirectory.Characteristics:=0;
     ImageExportDirectory.TimeDateStamp:=0;
     ImageExportDirectory.MajorVersion:=0;
     ImageExportDirectory.MinorVersion:=0;
     ImageExportDirectory.Name:=0;
     ImageExportDirectory.Base:=1;
     ImageExportDirectory.NumberOfFunctions:=ExportList.Count;
     ImageExportDirectory.NumberOfNames:=ExportList.Count;
     ImageExportDirectory.AddressOfFunctions:=0;
     ImageExportDirectory.AddressOfNames:=0;
     ImageExportDirectory.AddressOfNameOrdinals:=0;
     ExportSection.Stream.WriteBuffer(ImageExportDirectory,SizeOf(TImageExportDirectory));

     AddressOfFunctions:=ExportSection.Stream.Position;
     for ExportIndex:=0 to ExportList.Count-1 do begin
      if PassIndex=1 then begin
       TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,ExportList.Values[ExportList.Names[ExportIndex]],ExportSection,0,0,IMAGE_SYM_CLASS_EXTERNAL,plcpskUndefined);
       TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
       ExportSection.Symbols.Add(TempSectionSymbol);
       Relocation:=ExportSection.NewRelocation;
       Relocation^.VirtualAddress:=ExportSection.Stream.Position;
       Relocation^.Symbol:=TempSectionSymbolIndex;
       case fMachine of
        IMAGE_FILE_MACHINE_I386:begin
         Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
        end;
        else begin
         Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
        end;
       end;
      end;
      Value:=0;
      ExportSection.Stream.WriteBuffer(Value,SizeOf(TPACCUInt32));
     end;

     AddressOfNames:=ExportSection.Stream.Position;
     Value:=ExportSection.Stream.Position+(ExportList.Count*SizeOf(TPACCUInt32));
     for ExportIndex:=0 to ExportList.Count-1 do begin
      if PassIndex=1 then begin
       TempSectionSymbol:=TPACCLinker_COFF_PE_Symbol.Create(self,'@@__export_data_section_addressof_AddressOfName_'+IntToStr(ExportIndex),ExportSection,Value,0,IMAGE_SYM_CLASS_STATIC,plcpskNormal);
       TempSectionSymbolIndex:=Symbols.Add(TempSectionSymbol);
       ExportSection.Symbols.Add(TempSectionSymbol);
       Relocation:=ExportSection.NewRelocation;
       Relocation^.VirtualAddress:=ExportSection.Stream.Position;
       Relocation^.Symbol:=TempSectionSymbolIndex;
       case fMachine of
        IMAGE_FILE_MACHINE_I386:begin
         Relocation^.RelocationType:=IMAGE_REL_I386_DIR32NB;
        end;
        else begin
         Relocation^.RelocationType:=IMAGE_REL_AMD64_ADDR32NB;
        end;
       end;
      end;
      ExportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt32));
      inc(Value,length(ExportList.Names[ExportIndex])+1);
     end;
     for ExportIndex:=0 to ExportList.Count-1 do begin
      ExportName:=ExportList.Names[ExportIndex];
      ExportSection.Stream.WriteBuffer(ExportName[1],length(ExportName));
      ExportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt8));
     end;

     AddressOfOrdinals:=ExportSection.Stream.Position;
     for ExportIndex:=0 to ExportList.Count-1 do begin
      Value16:=ExportIndex;
      ExportSection.Stream.WriteBuffer(Value16,SizeOf(TPACCUInt16));
     end;

     AddressOfName:=ExportSection.Stream.Position;
     ExportSection.Stream.WriteBuffer(NullBytes[0],SizeOf(TPACCUInt8));

    end;

    PECOFFDirectoryEntry^.Size:=ExportSection.Stream.Size-PECOFFDirectoryEntry^.Offset;

   end;

  finally
   ExportList.Free;
  end;

 end;
 procedure SortSections;
 var SectionIndex:TPACCInt32;
     Section:TPACCLinker_COFF_PE_Section;
     SectionOrder:TStringList;
 begin
  SectionOrder:=TStringList.Create;
  try
   SectionOrder.Add('.text');
   SectionOrder.Add('.data');
   SectionOrder.Add('.bss');
   SectionOrder.Add('.didat');
   SectionOrder.Add('.edata');
   SectionOrder.Add('.idata');
   SectionOrder.Add('.tls');
   SectionOrder.Add('.rdata');
   SectionOrder.Add('.pdata');
   SectionOrder.Add('.reloc');
   SectionOrder.Add('.rsrc');
   for SectionIndex:=0 to Sections.Count-1 do begin
    Section:=Sections[SectionIndex];
    Section.Order:=SectionOrder.IndexOf(Section.Name);
    if Section.Order<0 then begin
     Section.Order:=SectionOrder.Count;
    end;
   end;
  finally
   SectionOrder.Free;
  end;
  Sections.Sort(CompareSections);
 end;
 procedure MergeDuplicateAndDeleteUnusedSections;
  procedure AdjustSymbolsForSectionIndexToDelete(ToDeleteSectionIndex,NewSectionIndex:TPACCInt32);
  var SymbolIndex:TPACCInt32;
      Symbol:TPACCLinker_COFF_PE_Symbol;
  begin
   for SymbolIndex:=0 to Symbols.Count-1 do begin
    Symbol:=Symbols[SymbolIndex];
    if assigned(Symbol.fAuxData) and (Symbol.fAuxData.Size>0) then begin
     case Symbol.Class_ of
      IMAGE_SYM_CLASS_STATIC,IMAGE_SYM_CLASS_SECTION:begin
       if PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number>0 then begin
        if PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number=(ToDeleteSectionIndex+1) then begin
         PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number:=NewSectionIndex+1;
        end else if PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number>(ToDeleteSectionIndex+1) then begin
         dec(PImageAuxSymbol(Symbol.fAuxData.Memory)^.Section.Number);
        end;
       end;
      end;
     end;
    end;
   end;
  end;
 var SectionIndex,RelocationIndex,RelocationStartIndex,SymbolIndex,DestinationSectionIndex,Index:TPACCInt32;
     FillUpCount,StartOffset,VirtualAddressDelta:TPACCInt64;
     SectionNameHashMap:TPACCRawByteStringHashMap;
     Section,DestinationSection:TPACCLinker_COFF_PE_Section;
     Relocation:PPACCLinker_COFF_PE_Relocation;
     Symbol:TPACCLinker_COFF_PE_Symbol;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
 begin

  SectionNameHashMap:=TPACCRawByteStringHashMap.Create;
  try

   SectionIndex:=0;
   while SectionIndex<Sections.Count do begin

    Section:=Sections[SectionIndex];

    if Section.Active then begin

     DestinationSection:=SectionNameHashMap[Section.Name];

     if assigned(DestinationSection) then begin

      try

       DestinationSection.Stream.Seek(DestinationSection.Stream.Size,soBeginning);
       Section.Stream.Seek(0,soBeginning);
       StartOffset:=DestinationSection.Stream.Size;

       if Section.Alignment<>0 then begin
        FillUpCount:=((DestinationSection.Stream.Size+(Section.Alignment-1)) and not (Section.Alignment-1))-DestinationSection.Stream.Size;
        if FillUpCount>0 then begin
         if ((DestinationSection.Characteristics or Section.Characteristics) and IMAGE_SCN_CNT_CODE)<>0 then begin
          WriteNOPPadding(DestinationSection.Stream,FillUpCount);
         end else begin
          WriteNullPadding(DestinationSection.Stream,FillUpCount);
         end;
         StartOffset:=DestinationSection.Stream.Size;
        end;
       end;

       DestinationSection.Alignment:=Max(DestinationSection.Alignment,Section.Alignment);

       DestinationSection.Characteristics:=DestinationSection.Characteristics or Section.Characteristics;

       DestinationSection.Stream.CopyFrom(Section.Stream,Section.Stream.Size);

       VirtualAddressDelta:=(DestinationSection.VirtualAddress+StartOffset)-Section.VirtualAddress;

{$if true}
       RelocationStartIndex:=DestinationSection.CountRelocations;
       inc(DestinationSection.CountRelocations,Section.CountRelocations);
       if length(DestinationSection.Relocations)<DestinationSection.CountRelocations then begin
        SetLength(DestinationSection.Relocations,DestinationSection.CountRelocations*2);
       end;
       for RelocationIndex:=0 to Section.CountRelocations-1 do begin
        Relocation:=@DestinationSection.Relocations[RelocationStartIndex+RelocationIndex];
        Relocation^:=Section.Relocations[RelocationIndex];
        inc(Relocation^.VirtualAddress,VirtualAddressDelta);
       end;
{$else}
       for RelocationIndex:=0 to Section.CountRelocations-1 do begin
        Relocation:=DestinationSection.NewRelocation;
        Relocation^:=Section.Relocations[RelocationIndex];
        inc(Relocation^.VirtualAddress,VirtualAddressDelta);
       end;
{$ifend}

       for SymbolIndex:=0 to Section.Symbols.Count-1 do begin
        Symbol:=Section.Symbols[SymbolIndex];
        DestinationSection.Symbols.Add(Symbol);
        Symbol.Section:=DestinationSection;
        case Symbol.Class_ of
         IMAGE_SYM_CLASS_EXTERNAL,IMAGE_SYM_CLASS_STATIC:begin
          if (Symbol.SymbolKind=plcpskNormal) and assigned(Symbol.Section) then begin
           Symbol.Value:=Symbol.Value+StartOffset;
          end;
         end;
        end;
       end;

       for Index:=0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 do begin
        PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[Index];
        if PECOFFDirectoryEntry^.Section=Section then begin
         PECOFFDirectoryEntry^.Section:=DestinationSection;
         inc(PECOFFDirectoryEntry^.Offset,StartOffset);
         PECOFFDirectoryEntry^.Size:=0;
        end;
       end;

      finally
       DestinationSectionIndex:=Sections.IndexOf(DestinationSection);
       AdjustSymbolsForSectionIndexToDelete(SectionIndex,DestinationSectionIndex);
       Section.Free;
       Sections.Delete(SectionIndex);
      end;

     end else begin
      inc(SectionIndex);
      SectionNameHashMap[Section.Name]:=Section;
     end;

    end else begin
     AdjustSymbolsForSectionIndexToDelete(SectionIndex,-1);
     for Index:=0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 do begin
      PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[Index];
      if PECOFFDirectoryEntry^.Section=Section then begin
       PECOFFDirectoryEntry^.Section:=nil;
       PECOFFDirectoryEntry^.Offset:=0;
       PECOFFDirectoryEntry^.Size:=0;
      end;
     end;
     for SymbolIndex:=0 to Section.Symbols.Count-1 do begin
      Section.Symbols[SymbolIndex].Active:=false;
     end;
     Section.Free;
     Sections.Delete(SectionIndex);
    end;

   end;

  finally
   SectionNameHashMap.Free;
  end;

 end;
 procedure PositionAndSizeSections;
 var SectionIndex,RelocationIndex:TPACCInt32;
     Section:TPACCLinker_COFF_PE_Section;
     Relocation:PPACCLinker_COFF_PE_Relocation;
 begin
  LastVirtualAddress:=PECOFFSectionAlignment;
  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   LastVirtualAddress:=(LastVirtualAddress+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);
   Section.VirtualAddress:=LastVirtualAddress;
   Section.VirtualSize:=(Section.Stream.Size+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);
   Section.RawSize:=Section.Stream.Size;
   for RelocationIndex:=0 to Section.CountRelocations-1 do begin
    Relocation:=@Section.Relocations[RelocationIndex];
    inc(Relocation^.VirtualAddress,LastVirtualAddress);
   end;
   inc(LastVirtualAddress,Section.VirtualSize);
  end;
 end;
 procedure ResolveSymbols;
 var SymbolIndex:TPACCInt32;
     Symbol:TPACCLinker_COFF_PE_Symbol;
     UnresolvableExternalSymbols:boolean;
 begin
  UnresolvableExternalSymbols:=false;
  for SymbolIndex:=0 to Symbols.Count-1 do begin
   Symbol:=Symbols[SymbolIndex];
   if Symbol.Active and (Symbol.Class_=IMAGE_SYM_CLASS_EXTERNAL) and (Symbol.SymbolKind=plcpskNormal) and assigned(Symbol.Section) then begin
    if assigned(ExternalAvailableSymbolHashMap[Symbol.Name]) then begin
     if Symbol.Name<>'@feat.00' then begin
      TPACCInstance(Instance).AddWarning('Duplicate public symbol "'+Symbol.Name+'"',nil);
     end;
    end else begin
     ExternalAvailableSymbolHashMap[Symbol.Name]:=Symbol;
    end;
   end;
  end;
  for SymbolIndex:=0 to Symbols.Count-1 do begin
   Symbol:=Symbols[SymbolIndex];
   if Symbol.Active and (Symbol.Class_=IMAGE_SYM_CLASS_EXTERNAL) and
      ((Symbol.SymbolKind=plcpskUndefined) or
       ((Symbol.SymbolKind=plcpskNormal) and not assigned(Symbol.Section))) and
       not assigned(Symbol.Alias) then begin
    Symbol.Alias:=ExternalAvailableSymbolHashMap[Symbol.Name];
    if not assigned(Symbol.Alias) then begin
     UnresolvableExternalSymbols:=true;
     TPACCInstance(Instance).AddError('Unresolvable external symbol "'+Symbol.Name+'"',nil,false);
    end;
   end;
  end;
  if UnresolvableExternalSymbols then begin
   TPACCInstance(Instance).AddError('Unresolvable external symbols',nil,true);
  end;
 end;
 procedure ResolveRelocations;
 type PSectionBytes=^TSectionBytes;
      TSectionBytes=array[0..65535] of TPACCUInt8;
 var SectionIndex,RelocationIndex,SymbolIndex:TPACCInt32;
     Relocation:PPACCLinker_COFF_PE_Relocation;
     Section:TPACCLinker_COFF_PE_Section;
     Symbol:TPACCLinker_COFF_PE_Symbol;
     SymbolRVA,Offset,VirtualAddress:TPACCUInt64;
     SectionData:PSectionBytes;
 begin
  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   SectionData:=Section.Stream.Memory;
   for RelocationIndex:=0 to Section.CountRelocations-1 do begin
    Relocation:=@Section.Relocations[RelocationIndex];
    Offset:=Relocation^.VirtualAddress-Section.VirtualAddress;
    VirtualAddress:=Relocation^.VirtualAddress;
    SymbolRVA:=0;
    SymbolIndex:=Relocation^.Symbol;
    if (SymbolIndex>=0) and (SymbolIndex<Symbols.Count) then begin
     Symbol:=Symbols[SymbolIndex];
     if Symbol.Active then begin
      if assigned(Symbol.Alias) then begin
       Symbol:=Symbol.Alias;
      end;
      case Symbol.Class_ of
       IMAGE_SYM_CLASS_EXTERNAL,IMAGE_SYM_CLASS_STATIC:begin
        case Symbol.SymbolKind of
         plcpskAbsolute:begin
          SymbolRVA:=Symbol.Value;
         end;
         plcpskNormal:begin
          if assigned(Symbol.Section) then begin
           SymbolRVA:=Symbol.Section.VirtualAddress+Symbol.Value;
          end else begin
           TPACCInstance(Instance).AddError('Invalid symbol "'+Symbol.Name+'"',nil,true);
          end;
         end;
         else begin
          TPACCInstance(Instance).AddError('Symbol "'+Symbol.Name+'" is not applicable for relocation',nil,true);
         end;
        end;
       end;
       else begin
        TPACCInstance(Instance).AddError('Symbol "'+Symbol.Name+'" is not applicable for relocation',nil,true);
       end;
      end;
     end else begin
      TPACCInstance(Instance).AddError('Symbol "'+Symbol.Name+'" is not applicable for relocation, because it is stripped out',nil,true);
     end;
    end else begin
     Symbol:=nil;
    end;
    case fMachine of
     IMAGE_FILE_MACHINE_I386:begin
      case Relocation^.RelocationType of
       IMAGE_REL_I386_ABSOLUTE:begin
        // ignore
       end;
       IMAGE_REL_I386_DIR16:begin
        inc(PPACCUInt16(pointer(@SectionData^[Offset]))^,(ImageBase+SymbolRVA) and $ffff);
        RelocationsAdd(Relocations,VirtualAddress,IMAGE_REL_BASED_LOW);
       end;
       IMAGE_REL_I386_REL16:begin
        inc(PPACCUInt16(pointer(@SectionData^[Offset]))^,(SymbolRVA-(VirtualAddress+4)) and $ffff);
       end;
       IMAGE_REL_I386_DIR32:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,ImageBase+SymbolRVA);
        RelocationsAdd(Relocations,VirtualAddress,IMAGE_REL_BASED_HIGHLOW);
       end;
       IMAGE_REL_I386_DIR32NB:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA);
       end;
       IMAGE_REL_I386_SEG12:begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
       IMAGE_REL_I386_SECTION:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         inc(PPACCUInt16(pointer(@SectionData^[Offset]))^,Sections.IndexOf(Symbol.Section));
        end else begin
         TPACCInstance(Instance).AddError('SECTION relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_I386_SECREL:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-Symbol.Section.VirtualAddress);
        end else begin
         TPACCInstance(Instance).AddError('SECREL relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_I386_TOKEN:begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
       IMAGE_REL_I386_SECREL7:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         PPACCUInt8(pointer(@SectionData^[Offset]))^:=(((PPACCUInt8(pointer(@SectionData^[Offset]))^ and $7f)+(SymbolRVA-Symbol.Section.VirtualAddress)) and $7f) or (PPACCUInt8(pointer(@SectionData^[Offset]))^ and $80);
        end else begin
         TPACCInstance(Instance).AddError('SECREL7 relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_I386_REL32:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+4));
       end;
       else begin
        writeln(Relocation^.RelocationType);
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
      end;
     end;
     IMAGE_FILE_MACHINE_AMD64:begin
      case Relocation^.RelocationType of
       IMAGE_REL_AMD64_ABSOLUTE:begin
        // ignore
       end;
       IMAGE_REL_AMD64_ADDR64:begin
        inc(PPACCUInt64(pointer(@SectionData^[Offset]))^,ImageBase+SymbolRVA);
        RelocationsAdd(Relocations,VirtualAddress,IMAGE_REL_BASED_DIR64);
       end;
       IMAGE_REL_AMD64_ADDR32:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,ImageBase+SymbolRVA);
        RelocationsAdd(Relocations,VirtualAddress,IMAGE_REL_BASED_HIGHLOW);
       end;
       IMAGE_REL_AMD64_ADDR32NB:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA);
       end;
       IMAGE_REL_AMD64_REL32:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+4));
       end;
       IMAGE_REL_AMD64_REL32_1:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+5));
       end;
       IMAGE_REL_AMD64_REL32_2:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+6));
       end;
       IMAGE_REL_AMD64_REL32_3:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+7));
       end;
       IMAGE_REL_AMD64_REL32_4:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+8));
       end;
       IMAGE_REL_AMD64_REL32_5:begin
        inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-(VirtualAddress+9));
       end;
       IMAGE_REL_AMD64_SECTION:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         inc(PPACCUInt16(pointer(@SectionData^[Offset]))^,Sections.IndexOf(Symbol.Section));
        end else begin
         TPACCInstance(Instance).AddError('SECTION relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_AMD64_SECREL:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-Symbol.Section.VirtualAddress);
        end else begin
         TPACCInstance(Instance).AddError('SECREL relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_AMD64_SECREL7:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         PPACCUInt8(pointer(@SectionData^[Offset]))^:=(((PPACCUInt8(pointer(@SectionData^[Offset]))^ and $7f)+(SymbolRVA-Symbol.Section.VirtualAddress)) and $7f) or (PPACCUInt8(pointer(@SectionData^[Offset]))^ and $80);
        end else begin
         TPACCInstance(Instance).AddError('SECREL7 relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_AMD64_TOKEN:begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
       IMAGE_REL_AMD64_SREL32:begin
        if assigned(Symbol) and assigned(Symbol.Section) then begin
         inc(PPACCUInt32(pointer(@SectionData^[Offset]))^,SymbolRVA-Symbol.Section.VirtualAddress);
        end else begin
         TPACCInstance(Instance).AddError('SREL32 relocation points to a non-regular symbol',nil,true);
        end;
       end;
       IMAGE_REL_AMD64_PAIR:begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
       IMAGE_REL_AMD64_SSPAN32:begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
       IMAGE_REL_AMD64_ADDR64NB:begin
        inc(PPACCUInt64(pointer(@SectionData^[Offset]))^,SymbolRVA);
       end;
       else begin
        TPACCInstance(Instance).AddError('Unsupported relocation',nil,true);
       end;
      end;
     end;
    end;
   end;
  end;
 end;
 procedure GenerateRelocationSection;
 var SectionIndex:TPACCInt32;
     Section:TPACCLinker_COFF_PE_Section;
     Size:TPACCUInt32;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
 begin
  if TPACCInstance(Instance).Options.CreateSharedLibrary and assigned(Relocations.RootNode) then begin

   RelocationsSort(Relocations);

   Size:=RelocationsSize(Relocations);

   for SectionIndex:=0 to Sections.Count-1 do begin
    Section:=Sections[SectionIndex];
    if Section.Name='.reloc' then begin
     TPACCInstance(Instance).AddError('Section ".reloc" already exist',nil,true);
    end;
   end;

   Section:=TPACCLinker_COFF_PE_Section.Create(self,'.reloc',0,IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ);
   Sections.Add(Section);

   LastVirtualAddress:=(LastVirtualAddress+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);
   Section.VirtualAddress:=LastVirtualAddress;

   Section.Stream.SetSize(Size);
   RelocationsBuild(Relocations,Section.Stream.Memory,0);

   Section.VirtualSize:=(Section.Stream.Size+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);

   Section.RawSize:=Section.Stream.Size;
   while (Section.RawSize>0) and (TPACCUInt8(PAnsiChar(Section.Stream.Memory)[Section.RawSize-1])=0) do begin
    Section.RawSize:=Section.RawSize-1;
   end;

   inc(LastVirtualAddress,Section.VirtualSize);

   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_BASERELOC];
   PECOFFDirectoryEntry^.Section:=Section;
   PECOFFDirectoryEntry^.Offset:=0;
   PECOFFDirectoryEntry^.Size:=Size;

  end;
 end;
 procedure GenerateResourceSection;
 type TPart=(ptNodes,ptDataEntries,ptNames,ptData);
 var SectionIndex,ResourceIndex,Index,PassIndex:TPACCInt32;
     Section:TPACCLinker_COFF_PE_Section;
     CurrentObject:TObject;
     Part:TPart;
     Pass:boolean;
     Root,Node:TResourceNode;
     Item:TResourceNodeItem;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
     Resource:TPACCLinker_COFF_PE_Resource;
     Stack,ChildNodes:TList;
     ImageResourceDirectory:TImageResourceDirectory;
     ImageResourceDirectoryEntry:TImageResourceDirectoryEntry;
     ImageResourceDataEntry:TImageResourceDataEntry;
     v16:TPACCUInt16;
 begin
  if Resources.Count>0 then begin

   for SectionIndex:=0 to Sections.Count-1 do begin
    Section:=Sections[SectionIndex];
    if Section.Name='.rsrc' then begin
     TPACCInstance(Instance).AddError('Section ".rsrc" already exist',nil,true);
    end;
   end;

   Section:=TPACCLinker_COFF_PE_Section.Create(self,'.rsrc',0,IMAGE_SCN_CNT_INITIALIZED_DATA or IMAGE_SCN_MEM_READ or IMAGE_SCN_CNT_RESOURCE);
   Sections.Add(Section);

   LastVirtualAddress:=(LastVirtualAddress+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);
   Section.VirtualAddress:=LastVirtualAddress;

   Root:=nil;
   try

    for ResourceIndex:=0 to Resources.Count-1 do begin
     Resource:=Resources[ResourceIndex];
     if assigned(Root) then begin
      Root.Add(Resource.Type_,Resource.Name,Resource.fLanguageID,Resource.Stream,Resource.CodePage);
     end else begin
      Root:=TResourceNode.Create(Resource.Type_,Resource.Name,Resource.fLanguageID,Resource.Stream,Resource.CodePage);
     end;
    end;

    Stack:=TList.Create;
    try

     ChildNodes:=TList.Create;
     try

      for PassIndex:=0 to 1 do begin

       Section.Stream.Seek(0,soBeginning);

       for Part:=ptNodes to ptData do begin

        case Part of
         ptDataEntries:begin
          if (Section.Stream.Position and not TPACCInt64(3))<>0 then begin
           WriteNullPadding(Section.Stream,((Section.Stream.Position+3) and not TPACCInt64(3))-Section.Stream.Position);
          end;
         end;
         ptNames:begin
          if (Section.Stream.Position and not TPACCInt64(3))<>0 then begin
           WriteNullPadding(Section.Stream,((Section.Stream.Position+3) and not TPACCInt64(3))-Section.Stream.Position);
          end;
         end;
         ptData:begin
          if (Section.Stream.Position and not TPACCInt64(15))<>0 then begin
           WriteNullPadding(Section.Stream,((Section.Stream.Position+15) and not TPACCInt64(15))-Section.Stream.Position);
          end;
         end;
        end;

        Stack.Add(Root);
        while Stack.Count>0 do begin
         Node:=Stack[Stack.Count-1];
         Stack.Delete(Stack.Count-1);
         ChildNodes.Clear;
         case Part of
          ptNodes:begin
           FillChar(ImageResourceDirectory,SizeOf(TImageResourceDirectory),#0);
           ImageResourceDirectory.MajorVersion:=4;
           ImageResourceDirectory.MinorVersion:=0;
           ImageResourceDirectory.NumberOfNamedEntries:=0;
           ImageResourceDirectory.NumberOfIDEntries:=0;
           for Index:=0 to Node.Count-1 do begin
            Item:=Node[Index];
            if Item.IsIntegerID then begin
             inc(ImageResourceDirectory.NumberOfIDEntries);
            end else begin
             inc(ImageResourceDirectory.NumberOfNamedEntries);
            end;
           end;
           Node.Offset:=Section.Stream.Position;
           Section.Stream.WriteBuffer(ImageResourceDirectory,SizeOf(TImageResourceDirectory));
          end;
         end;
         for Pass:=true downto false do begin
          for Index:=0 to Node.Count-1 do begin
           Item:=Node[Index];
           if Item.IsIntegerID xor Pass then begin
            case Part of
             ptNodes:begin
              if Item.IsIntegerID then begin
               ImageResourceDirectoryEntry.TypeNameLevel:=StrToInt(String(Item.ID));
              end else begin
               ImageResourceDirectoryEntry.TypeNameLevel:=Item.NameOffset or $80000000;
              end;
              if Item.Leaf then begin
               ImageResourceDirectoryEntry.OffsetToChildData:=Item.DataEntryOffset;
              end else begin
               ImageResourceDirectoryEntry.OffsetToChildData:=Item.Node.Offset or $80000000;
              end;
              Section.Stream.WriteBuffer(ImageResourceDirectoryEntry,SizeOf(TImageResourceDirectoryEntry));
             end;
             ptDataEntries:begin
              if Item.Leaf then begin
               Item.DataEntryOffset:=Section.Stream.Position;
               FillChar(ImageResourceDataEntry,SizeOf(TImageResourceDataEntry),#0);
               ImageResourceDataEntry.OffsetToData:=Item.DataRVA;
               ImageResourceDataEntry.Size:=Item.Stream.Size;
               ImageResourceDataEntry.CodePage:=Item.CodePage;
               ImageResourceDataEntry.Reserved:=0;
               Section.Stream.WriteBuffer(ImageResourceDataEntry,SizeOf(TImageResourceDataEntry));
              end else begin
               Item.DataEntryOffset:=0;
              end;
             end;
             ptNames:begin
              if Item.IsIntegerID then begin
               Item.NameOffset:=0;
              end else begin
               Item.NameOffset:=Section.Stream.Position;
               v16:=length(Item.ID);
               Section.Stream.WriteBuffer(v16,SizeOf(TPACCUInt16));
               if v16>0 then begin
                Section.Stream.WriteBuffer(Item.ID[1],v16*SizeOf(TPUCUUTF16Char));
               end;
              end;
             end;
             ptData:begin
              if Item.Leaf then begin
               Item.DataRVA:=Section.VirtualAddress+Section.Stream.Position;
               Item.Stream.Seek(0,soBeginning);
               Section.Stream.CopyFrom(Item.Stream,Item.Stream.Size);
               if (Section.Stream.Position and not TPACCInt64(3))<>0 then begin
                WriteNullPadding(Section.Stream,((Section.Stream.Position+3) and not TPACCInt64(3))-Section.Stream.Position);
               end;
              end else begin
               Item.DataRVA:=0;
              end;
             end;
            end;
            if not Item.Leaf then begin
             ChildNodes.Add(Item.Node);
            end;
           end;
          end;
         end;
         for Index:=ChildNodes.Count-1 downto 0 do begin
          Stack.Add(ChildNodes[Index]);
         end;
        end;

        case Part of
         ptData:begin
          if (Section.Stream.Position and not TPACCInt64(15))<>0 then begin
           WriteNullPadding(Section.Stream,((Section.Stream.Position+15) and not TPACCInt64(15))-Section.Stream.Position);
          end;
         end;
        end;

       end;

      end;

     finally
      ChildNodes.Free;
     end;

    finally
     Stack.Free;
    end;

   finally
    Root.Free;
   end;

   Section.VirtualSize:=(Section.Stream.Size+(PECOFFSectionAlignment-1)) and not TPACCInt64(PECOFFSectionAlignment-1);

   Section.RawSize:=Section.Stream.Size;
{  while (Section.RawSize>0) and (TPACCUInt8(PAnsiChar(Section.Stream.Memory)[Section.RawSize-1])=0) do begin
    Section.RawSize:=Section.RawSize-1;
   end;}

   inc(LastVirtualAddress,Section.VirtualSize);

   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_RESOURCE];
   PECOFFDirectoryEntry^.Section:=Section;
   PECOFFDirectoryEntry^.Offset:=0;
   PECOFFDirectoryEntry^.Size:=Section.Stream.Size;

  end;
 end;
 procedure GenerateImage(const Stream:TStream);
 type PSectionBytes=^TSectionBytes;
      TSectionBytes=array[0..65535] of TPACCUInt8;
 var Index,HeaderSize,SectionIndex,Len:TPACCInt32;
     Characteristics,TotalImageSize,AddressOfEntryPoint,CodeBase,SubSystem,
     DLLCharacteristics,SizeOfStackReserve,SizeOfStackCommit,
     SizeOfHeapReserve,SizeOfHeapCommit:TPACCUInt32;
     ImageNTHeaders:TImageNTHeaders;
     PECOFFDirectoryEntry:PPECOFFDirectoryEntry;
     FileOffset,CountBytes,TempSize:TPACCInt64;
     Section:TPACCLinker_COFF_PE_Section;
     ImageSectionHeader:TImageSectionHeader;
     Symbol:TPACCLinker_COFF_PE_Symbol;
 begin

  Characteristics:=IMAGE_FILE_EXECUTABLE_IMAGE or
                   IMAGE_FILE_LINE_NUMS_STRIPPED or
                   IMAGE_FILE_LOCAL_SYMS_STRIPPED or
                   IMAGE_FILE_DEBUG_STRIPPED or
                   IMAGE_FILE_LARGE_ADDRESS_AWARE;
  if not Is64Bit then begin
   Characteristics:=Characteristics or IMAGE_FILE_32BIT_MACHINE;
  end;
  if TPACCInstance(Instance).Options.CreateSharedLibrary then begin
   Characteristics:=Characteristics or IMAGE_FILE_DLL;
   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXPORT];
   if not (assigned(PECOFFDirectoryEntry^.Section) and (PECOFFDirectoryEntry^.Size>0)) then begin
    TPACCInstance(Instance).AddWarning('DLL without exports',nil);
   end;
  end else begin
   Characteristics:=Characteristics or IMAGE_FILE_RELOCS_STRIPPED;
  end;

  TotalImageSize:=LastVirtualAddress;

  Symbol:=ExternalAvailableSymbolHashMap['_start'];
  if assigned(Symbol) and assigned(Symbol.Section) then begin
   AddressOfEntryPoint:=Symbol.Section.VirtualAddress+Symbol.Value;
  end else begin
   AddressOfEntryPoint:=0;
  end;

  CodeBase:=PECOFFSectionAlignment;

  Symbol:=ExternalAvailableSymbolHashMap['WinMain'];
  if assigned(Symbol) and assigned(Symbol.Section) then begin
   SubSystem:=IMAGE_SUBSYSTEM_WINDOWS_GUI;
  end else begin
   SubSystem:=IMAGE_SUBSYSTEM_WINDOWS_CUI;
  end;

  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   if (Section.Name='.edata') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXPORT].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXPORT];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.idata') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_IMPORT].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_IMPORT];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.didat') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT_DESCRIPTOR].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_DELAY_IMPORT_DESCRIPTOR];
    PECOFFDirectoryEntry^.Section:=Section;             
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.dbgdata') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_DEBUG].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_DEBUG];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.rsrc') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_RESOURCE].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_RESOURCE];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.pdata') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXCEPTION].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_EXCEPTION];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end else if (Section.Name='.reloc') and (not assigned(PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_BASERELOC].Section)) and (Section.RawSize>0) then begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_BASERELOC];
    PECOFFDirectoryEntry^.Section:=Section;
    PECOFFDirectoryEntry^.Offset:=0;
    PECOFFDirectoryEntry^.Size:=Section.Stream.Size;
   end;
  end;

  Symbol:=ExternalAvailableSymbolHashMap['_tls_used'];
  if assigned(Symbol) and assigned(Symbol.Section) then begin
   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_TLS];
   PECOFFDirectoryEntry^.Section:=Symbol.Section;
   PECOFFDirectoryEntry^.Offset:=Symbol.Value;
   if Is64Bit then begin
    PECOFFDirectoryEntry^.Size:=SizeOf(TImageTLSDirectory64);
   end else begin
    PECOFFDirectoryEntry^.Size:=SizeOf(TImageTLSDirectory32);
   end;
  end;

  Symbol:=ExternalAvailableSymbolHashMap['_load_config_used'];
  if assigned(Symbol) and assigned(Symbol.Section) then begin
   PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[IMAGE_DIRECTORY_ENTRY_LOAD_CONFIG];
   if (Symbol.Value+4)>Symbol.Section.Stream.Size then begin
    TPACCInstance(Instance).AddError('_load_config_used is malformed',nil,true);
   end else begin                             
    TempSize:=PPACCUInt32(pointer(@PSectionBytes(Symbol.Section.Stream)^[Symbol.Value]))^;
    if (Symbol.Value+TempSize)>Symbol.Section.Stream.Size then begin
     TPACCInstance(Instance).AddError('_load_config_used is too large',nil,true);
    end else begin
     PECOFFDirectoryEntry^.Section:=Symbol.Section;
     PECOFFDirectoryEntry^.Offset:=Symbol.Value;
     PECOFFDirectoryEntry^.Size:=TempSize;
    end;
   end;
  end;

  DLLCharacteristics:=0;

  SizeOfStackReserve:=$100000;

  SizeOfStackCommit:=$2000;

  SizeOfHeapReserve:=$100000;

  SizeOfHeapCommit:=$2000;

  Stream.Size:=0;
  Stream.Seek(0,soBeginning);

  HeaderSize:=MZEXEHeaderSize+SizeOf(TPACCUInt32)+SizeOf(TImageFileHeader);
  if Is64Bit then begin
   inc(HeaderSize,SizeOf(TImageOptionalHeader64));
  end else begin
   inc(HeaderSize,SizeOf(TImageOptionalHeader));
  end;
  inc(HeaderSize,SizeOf(TImageSectionHeader)*Sections.Count);
  if (HeaderSize and (PECOFFFileAlignment-1))<>0 then begin
   HeaderSize:=(HeaderSize+(PECOFFFileAlignment-1)) and not (PECOFFFileAlignment-1);
  end;

  Stream.Write(MZEXEHeaderBytes[0],MZEXEHeaderSize);

  ImageNTHeaders.Signature:=$00004550;
  ImageNTHeaders.FileHeader.Machine:=fMachine;
  ImageNTHeaders.FileHeader.NumberOfSections:=Sections.Count;
  ImageNTHeaders.FileHeader.TimeDateStamp:=0;
  ImageNTHeaders.FileHeader.PointerToSymbolTable:=0;
  ImageNTHeaders.FileHeader.NumberOfSymbols:=0;
  if Is64Bit then begin
   ImageNTHeaders.FileHeader.SizeOfOptionalHeader:=SizeOf(TImageOptionalHeader64);
  end else begin
   ImageNTHeaders.FileHeader.SizeOfOptionalHeader:=SizeOf(TImageOptionalHeader);
  end;
  ImageNTHeaders.FileHeader.Characteristics:=Characteristics;

  Stream.Write(ImageNTHeaders.Signature,SizeOf(TPACCUInt32));
  Stream.Write(ImageNTHeaders.FileHeader,SizeOf(TImageFileHeader));

  if Is64Bit then begin
   ImageNTHeaders.OptionalHeader64.Magic:=$020b;
   ImageNTHeaders.OptionalHeader64.MajorLinkerVersion:=2;
   ImageNTHeaders.OptionalHeader64.MinorLinkerVersion:=50;
   ImageNTHeaders.OptionalHeader64.SizeOfCode:=TotalImageSize;
   ImageNTHeaders.OptionalHeader64.SizeOfInitializedData:=0;
   ImageNTHeaders.OptionalHeader64.SizeOfUninitializedData:=0;
   ImageNTHeaders.OptionalHeader64.AddressOfEntryPoint:=AddressOfEntryPoint;
   ImageNTHeaders.OptionalHeader64.BaseOfCode:=CodeBase;
   ImageNTHeaders.OptionalHeader64.ImageBase:=ImageBase;
   ImageNTHeaders.OptionalHeader64.SectionAlignment:=PECOFFSectionAlignment;
   ImageNTHeaders.OptionalHeader64.FileAlignment:=PECOFFFileAlignment;
   ImageNTHeaders.OptionalHeader64.MajorOperatingSystemVersion:=1;
   ImageNTHeaders.OptionalHeader64.MinorOperatingSystemVersion:=0;
   ImageNTHeaders.OptionalHeader64.MajorImageVersion:=0;
   ImageNTHeaders.OptionalHeader64.MinorImageVersion:=0;
   ImageNTHeaders.OptionalHeader64.MajorSubsystemVersion:=4;
   ImageNTHeaders.OptionalHeader64.MinorSubsystemVersion:=0;
   ImageNTHeaders.OptionalHeader64.Win32VersionValue:=0;
   ImageNTHeaders.OptionalHeader64.SizeOfImage:=SectionSizeAlign(TotalImageSize);
   ImageNTHeaders.OptionalHeader64.SizeOfHeaders:=HeaderSize;
   ImageNTHeaders.OptionalHeader64.CheckSum:=0;
   ImageNTHeaders.OptionalHeader64.Subsystem:=SubSystem;
   ImageNTHeaders.OptionalHeader64.DLLCharacteristics:=DLLCharacteristics;
   ImageNTHeaders.OptionalHeader64.SizeOfStackReserve:=SizeOfStackReserve;
   ImageNTHeaders.OptionalHeader64.SizeOfStackCommit:=SizeOfStackCommit;
   ImageNTHeaders.OptionalHeader64.SizeOfHeapReserve:=SizeOfHeapReserve;
   ImageNTHeaders.OptionalHeader64.SizeOfHeapCommit:=SizeOfHeapCommit;
   ImageNTHeaders.OptionalHeader64.LoaderFlags:=0;
   ImageNTHeaders.OptionalHeader64.NumberOfRvaAndSizes:=IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
   for Index:=0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 do begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[Index];
    if assigned(PECOFFDirectoryEntry^.Section) and (PECOFFDirectoryEntry^.Size>0) then begin
     ImageNTHeaders.OptionalHeader64.DataDirectory[Index].VirtualAddress:=PECOFFDirectoryEntry^.Section.VirtualAddress+PECOFFDirectoryEntry^.Offset;
     ImageNTHeaders.OptionalHeader64.DataDirectory[Index].Size:=PECOFFDirectoryEntry^.Size;
    end else begin
     ImageNTHeaders.OptionalHeader64.DataDirectory[Index].VirtualAddress:=0;
     ImageNTHeaders.OptionalHeader64.DataDirectory[Index].Size:=0;
    end;
   end;
   Stream.Write(ImageNTHeaders.OptionalHeader64,SizeOf(TImageOptionalHeader64));
  end else begin
   ImageNTHeaders.OptionalHeader.Magic:=$010b;
   ImageNTHeaders.OptionalHeader.MajorLinkerVersion:=2;
   ImageNTHeaders.OptionalHeader.MinorLinkerVersion:=50;
   ImageNTHeaders.OptionalHeader.SizeOfCode:=TotalImageSize;
   ImageNTHeaders.OptionalHeader.SizeOfInitializedData:=0;
   ImageNTHeaders.OptionalHeader.SizeOfUninitializedData:=0;
   ImageNTHeaders.OptionalHeader.AddressOfEntryPoint:=AddressOfEntryPoint;
   ImageNTHeaders.OptionalHeader.BaseOfCode:=CodeBase;
   ImageNTHeaders.OptionalHeader.BaseOfData:=0;
   ImageNTHeaders.OptionalHeader.ImageBase:=ImageBase;
   ImageNTHeaders.OptionalHeader.SectionAlignment:=PECOFFSectionAlignment;
   ImageNTHeaders.OptionalHeader.FileAlignment:=PECOFFFileAlignment;
   ImageNTHeaders.OptionalHeader.MajorOperatingSystemVersion:=1;
   ImageNTHeaders.OptionalHeader.MinorOperatingSystemVersion:=0;
   ImageNTHeaders.OptionalHeader.MajorImageVersion:=0;
   ImageNTHeaders.OptionalHeader.MinorImageVersion:=0;
   ImageNTHeaders.OptionalHeader.MajorSubsystemVersion:=4;
   ImageNTHeaders.OptionalHeader.MinorSubsystemVersion:=0;
   ImageNTHeaders.OptionalHeader.Win32VersionValue:=0;
   ImageNTHeaders.OptionalHeader.SizeOfImage:=SectionSizeAlign(TotalImageSize);
   ImageNTHeaders.OptionalHeader.SizeOfHeaders:=HeaderSize;
   ImageNTHeaders.OptionalHeader.CheckSum:=0;
   ImageNTHeaders.OptionalHeader.Subsystem:=SubSystem;
   ImageNTHeaders.OptionalHeader.DLLCharacteristics:=DLLCharacteristics;
   ImageNTHeaders.OptionalHeader.SizeOfStackReserve:=SizeOfStackReserve;
   ImageNTHeaders.OptionalHeader.SizeOfStackCommit:=SizeOfStackCommit;
   ImageNTHeaders.OptionalHeader.SizeOfHeapReserve:=SizeOfHeapReserve;
   ImageNTHeaders.OptionalHeader.SizeOfHeapCommit:=SizeOfHeapCommit;
   ImageNTHeaders.OptionalHeader.LoaderFlags:=0;
   ImageNTHeaders.OptionalHeader.NumberOfRvaAndSizes:=IMAGE_NUMBEROF_DIRECTORY_ENTRIES;
   for Index:=0 to IMAGE_NUMBEROF_DIRECTORY_ENTRIES-1 do begin
    PECOFFDirectoryEntry:=@PECOFFDirectoryEntries^[Index];
    if assigned(PECOFFDirectoryEntry^.Section) and (PECOFFDirectoryEntry^.Size>0) then begin
     ImageNTHeaders.OptionalHeader.DataDirectory[Index].VirtualAddress:=PECOFFDirectoryEntry^.Section.VirtualAddress+PECOFFDirectoryEntry^.Offset;
     ImageNTHeaders.OptionalHeader.DataDirectory[Index].Size:=PECOFFDirectoryEntry^.Size;
    end else begin
     ImageNTHeaders.OptionalHeader.DataDirectory[Index].VirtualAddress:=0;
     ImageNTHeaders.OptionalHeader.DataDirectory[Index].Size:=0;
    end;
   end;
   Stream.Write(ImageNTHeaders.OptionalHeader,SizeOf(TImageOptionalHeader));
  end;

  FileOffset:=HeaderSize;
  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   Section.FileOffset:=FileOffset;
   if ((Section.Characteristics and IMAGE_SCN_CNT_INITIALIZED_DATA)<>0) and (Section.RawSize=0) then begin
    Section.Characteristics:=(Section.Characteristics and not IMAGE_SCN_CNT_INITIALIZED_DATA) or IMAGE_SCN_CNT_UNINITIALIZED_DATA;
   end;
   if (Section.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA)=0 then begin
    TempSize:=FileSizeAlign(Section.RawSize);
    while (Section.RawSize>TempSize) and (TPACCUInt8(PAnsiChar(Section.Stream.Memory)[Section.RawSize-1])=0) do begin
     Section.RawSize:=Section.RawSize-1;
    end;
    inc(FileOffset,Section.RawSize);
    if (FileOffset and (PECOFFFileAlignment-1))<>0 then begin
     FileOffset:=(FileOffset+(PECOFFFileAlignment-1)) and not (PECOFFFileAlignment-1);
    end;
   end;
  end;
  if (FileOffset and (PECOFFFileAlignment-1))<>0 then begin
   FileOffset:=(FileOffset+(PECOFFFileAlignment-1)) and not (PECOFFFileAlignment-1);
  end;

  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   FillChar(ImageSectionHeader,SizeOf(TImageSectionHeader),#0);
   Len:=length(Section.Name);
   if Len>0 then begin
    if Len>8 then begin
     Len:=8;
    end;
    Move(Section.Name[1],ImageSectionHeader.Name[0],Len);
   end;
   ImageSectionHeader.Misc.VirtualSize:=Section.VirtualSize;
   ImageSectionHeader.VirtualAddress:=Section.VirtualAddress;
   if (Section.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA)<>0 then begin
    ImageSectionHeader.SizeOfRawData:=0;
    ImageSectionHeader.PointerToRawData:=0;
   end else begin
    ImageSectionHeader.SizeOfRawData:=FileSizeAlign(Section.RawSize);
    ImageSectionHeader.PointerToRawData:=Section.FileOffset;
   end;
   ImageSectionHeader.PointerToRelocations:=0;
   ImageSectionHeader.PointerToLineNumbers:=0;
   ImageSectionHeader.NumberOfRelocations:=0;
   ImageSectionHeader.NumberOfLineNumbers:=0;
   ImageSectionHeader.Characteristics:=Section.Characteristics;
   Stream.Write(ImageSectionHeader,SizeOf(TImageSectionHeader));
  end;

  CountBytes:=HeaderSize-Stream.Position;
  if CountBytes>0 then begin
   Stream.WriteBuffer(NullBytes[0],CountBytes);
  end;

  for SectionIndex:=0 to Sections.Count-1 do begin
   Section:=Sections[SectionIndex];
   if (Section.Characteristics and IMAGE_SCN_CNT_UNINITIALIZED_DATA)=0 then begin
    CountBytes:=Section.FileOffset-Stream.Position;
    if CountBytes>0 then begin
     Stream.WriteBuffer(NullBytes[0],CountBytes);
    end;
    Section.Stream.Seek(0,soBeginning);
    Stream.CopyFrom(Section.Stream,Section.RawSize);
    CountBytes:=FileSizeAlign(Section.RawSize)-Section.RawSize;
    if CountBytes>0 then begin
     Stream.WriteBuffer(NullBytes[0],CountBytes);
    end;
   end;
  end;

 end;
begin
 RelocationsInit(Relocations);
 try
  GetMem(PECOFFDirectoryEntries,SizeOf(TPECOFFDirectoryEntries));
  try
   FillChar(PECOFFDirectoryEntries^,SizeOf(TPECOFFDirectoryEntries),#0);
   case fMachine of
    IMAGE_FILE_MACHINE_AMD64:begin
     Is64Bit:=true;
    end;
    else begin
     Is64Bit:=false;
    end;
   end;

   ScanImports;
   GenerateImports;

   ScanExports;
   GenerateExports;

   SortSections;

   MergeDuplicateAndDeleteUnusedSections;

   PositionAndSizeSections;

   ExternalAvailableSymbolHashMap:=TPACCRawByteStringHashMap.Create;
   try

    ResolveSymbols;

    ResolveRelocations;

    GenerateRelocationSection;

    GenerateResourceSection;

    GenerateImage(AOutputStream);

   finally
    ExternalAvailableSymbolHashMap.Free;
   end;

  finally
   FreeMem(PECOFFDirectoryEntries);
  end;
 finally
  RelocationsDone(Relocations);
 end;
end;

initialization
 FillChar(NullBytes,SizeOf(NullBytes),#0);
end.
