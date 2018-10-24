/// common functions used by most Synopse projects
// - this unit is a part of the freeware Synopse mORMot framework,
// licensed under a MPL/GPL/LGPL tri-license; version 1.17
unit SynCommons2;

{
    This file is part of Synopse framework.

    Synopse framework. Copyright (C) 2012 Arnaud Bouchez
      Synopse Informatique - http://synopse.info

  *** BEGIN LICENSE BLOCK *****
  Version: MPL 1.1/GPL 2.0/LGPL 2.1

  The contents of this file are subject to the Mozilla Public License Version
  1.1 (the "License"); you may not use this file except in compliance with
  the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
  for the specific language governing rights and limitations under the License.

  The Original Code is Synopse framework.

  The Initial Developer of the Original Code is Arnaud Bouchez.

  Portions created by the Initial Developer are Copyright (C) 2012
  the Initial Developer. All Rights Reserved.

  Contributor(s):
  Alternatively, the contents of this file may be used under the terms of
  either the GNU General Public License Version 2 or later (the "GPL"), or
  the GNU Lesser General Public License Version 2.1 or later (the "LGPL"),
  in which case the provisions of the GPL or the LGPL are applicable instead
  of those above. If you wish to allow use of your version of this file only
  under the terms of either the GPL or the LGPL, and not to allow others to
  use your version of this file under the terms of the MPL, indicate your
  decision by deleting the provisions above and replace them with the notice
  and other provisions required by the GPL or the LGPL. If you do not delete
  the provisions above, a recipient may use your version of this file under
  the terms of any one of the MPL, the GPL or the LGPL.

  ***** END LICENSE BLOCK *****

  Version 1.7
  - first public release, corresponding to SQLite3 Framework 1.7

  Version 1.8
  - includes Unitary Testing class and functions
  - bug fixed in WinAnsiBufferToUtf8() and all WinAnsi to UTF-8 encoding
    functions (issue identified thanks to new _UTF8 testing function)
  - bug fixed in val() under Delphi 2009/2010 for some values (issue identified
    thanks to new NumericalConversion testing function)
  - bug fixed in AnsiICompW() - used in SynPdf unit
  - ambiguous SameText() function rewritten as SameTextU() with UTF-8 decoding
  - TTextWriter class moved from SQLite3Commons to SynCommons
  - new JSONEncode and JSONDecode functions to directly encode or decode any
    content to/from a valid UTF-8 JSON object content
  - enhanced URLEncode() and URLDecode() functions
  - new ExtendedToStr/ExtendedToString functions
  - new tests added (mostly relative to the new functions or classes)

  Version 1.9
  - now compiles under CrossKylix, and tested under Linux
  - new JSONEncodeArray procedures, to create JSON array content from
    supplied Delphi arrays (handle RawUTF8 text, double or integer arrays)
  - new AddCSV methods in TTextWriter handling Delphi arrays to be added
    as Comma-Separated-Values (handle RawUTF8 text, double or integer arrays)
  - new definition of PtrInt/PtrUInt, to match NativeInt/NativeUInt types,
    available since Delphi 2007 - some code rewrite in order to avoid any
    implicit conversion from/to integer/cardinal

  Version 1.9.2
  - new StringReplaceChars function

  Version 1.10
  - code modifications to compile with Delphi 6 compiler (Delphi 5 failed due
    to some obscure compiler bugs in SynCrypto.pas)

  Version 1.11
  - fix some obscure Delphi 2009 bug according to NativeUInt :(
  - source code modified to be 7 bit Ansi (so will work with all encodings)
  - a lot of code refactoring for our internal fork of ZeosLib
    (e.g. ISO 8601 date time extracted from SQLite3Commons, QuotedStr..)
  - new TRawUTF8List class, which is able to emulate a TStringList with our
    native UTF-8 string type (cross-compiler, from Delphi 6 up to XE)
  - new TRawUTF8Stream class, to typecast a RawUTF8 into a TStream
  - new IsWow64 and SystemInfo global variables

  Version 1.12
  - fixed issue "JSON floats decimal separator depends on language settings"
  - new UTF8ToWideChar() overloaded function, with MaxDestChars parameter 
  - new FillIncreasing() procedure
  - now handle our 32/64-bit variable-length integer encoding, via new
    FromVarUInt32/64 and ToVarUInt32/64 functions
  - new TFileBufferReader and TFileBufferWriter objects, implementing very fast
    read/write access to huge files, with new 32/64-bit variable-length integer
    encoding and optimized storage of IDs or Offsets (used in TSynBigTable)
  - new function UnQuoteSQLString()
  - another review of Pos() calls in the code (now use our fast PosEx)
  - TSynMemoryStream now replaces TRawUTF8Stream class, with constructors using
    either an AnsiString, either a memory buffer
  - new TSynTable, TSynTableFieldProperties and TSynTableStatement classes
    to handle a generic database table storing field values using our SBF
    compact binary format (more compact than BSON, similar to Protocol Buffers)
  - new WinAnsiToUnicodeString and Ansi7ToString functions
  - new TTextWriter.AddBinToHex method
  - new CompareOperator() functions and associated TCompareOperator type
  - new IntToThousandString() function (used for TSynTests e.g.)
  - new CreateInternalWindow() for creating a GDI message handler in any object

  Version 1.13
  - unit now compiles and works with Delphi 5 compiler
  - new low-level RTTI functions for handling record types: RecordEquals,
    RecordSave, RecordSaveLength, RecordLoad, RecordClear and RecordCopy
  - new TDynArray object, which is a wrapper around any dynamic array: you can
    now access to the dynamic array using TList-like properties and methods,
    e.g. Count, Add, Insert, Delete, Clear, IndexOf, Find, Sort and some
    new methods like LoadFromStream, SaveToStream, LoadFrom and SaveTo which
    allow fast binary serialization of any dynamic array, even containing
    strings or records; a CreateOrderedIndex method is also available to
    create individual index according to the dynamic array content; and any
    dynamic array can be serialized as UTF-8 JSON via TTextWriter.AddDynArrayJSON
    and TDynArray.LoadFromJSON methods
  - introducing direct content filtering and validation using
    TSynFilterOrValidate dedicated classes, for both TSQLRecord and SynBigTable
  - filtering is handled via some TSynFilter classes - TSynFilterUpperCase,
    TSynFilterUpperCaseU, TSynFilterLowerCase, TSynFilterLowerCaseU and
    TSynFilterTrim e.g.
  - validation is handled via some TSynValidate classes - TSynValidateRest,
    TSynValidateIPAddress, TSynValidateEmail, TSynValidatePattern,
    TSynValidatePatternI, TSynValidateText, TSynValidatePassWord e.g.
  - dedicated TSynTableFieldProperties.Validate method for validation (e.g. a
    TSynValidateTableUniqueField instance is created if tfoUnique is in Options)
  - dedicated TSynTableFieldProperties.Filter method for filtering (using
    common TSynFilter classes, working at UTF-8 Text content)
  - faster implementation of Move() for Delphi versions with no FastCode inside
  - new ConvertCaseUTF8(), UpperCaseU(), LowerCaseU(), Int64ToUInt32(),
    GetCardinalDef(), IsValidEmail, IsValidIP4Address(), PatchCodePtrUInt(),
    GetCaptionFromClass(), GetDisplayNameFromClass(), DateTimeToIso8601Text()
    StrUInt32(), StringBufferToUtf8(), IsZero(), AddPrefixToCSV(), IntToString(),
    RawUTF8DynArrayEquals(), FromVarString(), GetBitCSV(), SetBitCSV()
    procedures or functions (with associated tests)
  - new grep-like IsMatch() function for basic pattern matching
  - new BinToBase64, Base64ToBin and IsBase64 *fast* conversion functions
    (with optimized assembler version, using CPU pipelining and lookup table)
  - introducing the GarbageCollector TObjectList for handling a global garbage
    collector for instances which must live during the whole executable process
    (used e.g. to avoid a memory leak for "class var" or such variables)
  - new TSynLog class to handle enhanced logging to any application, with
    exception handling (+stack trace) and customer-side performance profiling
  - new TSynMapFile class to retrieve debugging information from .map file (and
    able to save and read smaller .mab files) - used by TSynLog if available
  - new TSynTestsLogged test suit class, with automated test case logging
  - Windows version detection enhanced, now retrieving TWindowsVersion enumerate
  - great performance improvement in TSynTableFieldProperties for update process
  - added TMemoryMap and TSynMemoryStreamMapped to handle memory-mapped files
  - added TMemoryMapText class to fast handle big UTF-8 text files (like logs)
  - now TTextWriter can have a custom internal buffer size (default 4096 bytes)
  - now TFileBufferWriter and TFileVersion are regular classes, not an
    object/record any more (this was incoherent since Delphi 2010)
  - new TFileBufferReader.OpenFrom(Stream) and ReadRawUTF8 methods
  - now TSynCache will use faster TDynArrayHashed for its content hashing
  - new Escape: TTextWriterKind optional parameter for TTextWriter.Add()
  - new SynLZ related compression functions: FileSynLZ/FileUnSynLZ and
    StreamSynLZ/StreamUnSynLZ
  - source can now be parsed per all Delphi IDE pre-parser (dual declaration as
    record or object because of Delphi 2010 and up was not understood)
  - fixed issue in TSynTable.Data() method: ID was not set as expected
  - fixed issue in TSynTableFieldProperties: wrong constraint evaluation and
    index refresh at records update
  - fixed issue in ToVarUInt32Length/ToVarUInt32LengthWithData

  Version 1.14
  - fix issues with Curr64ToStr() and Curr64ToPChar() with negative amounts,
    with some speed enhancements and new associated tests
  - fixed issue in produced JSON stream using '=' instead of ':'
  - new DoubleToStr(), StrCurr64(), UnicodeBufferToString(),
    RawUnicodeToString(), FillChar(), UpperCopy255W(), GetCaptionFromEnum(),
    SortDynArrayUnicodeString(), SortDynArrayUnicodeStringI() functions

  Version 1.15
  - unit now tested with Delphi XE2 (32 Bit)
  - TSynLog now writes the elapsed time (in us) for Enter/Leave events, and
    will flush the log content to disk on any exception (for safety)
  - new sllTrace and sllWarning levels for TSynLog class
  - new TSynLog.DefaultExtension property (set to '.log' by default)
  - new TSynLogFile.LogProc[] property for customer-side method profiling,
    with LogProcSort method available for sorting the resulting array, and
    LogProcMerged property to merge the location name timing
  - new TSynMapFile.FindLocation method for high-level .map symbol access
  - TSynMapFile now handles huge .map file (bigger default in-memory buffer)
  - fix potential GPF issue in code using ConvertHexToBin[]
  - new TSynLog.EventCount method
  - new TMemoryMapText.LineContains method for fast case-insensitive search
  - TSynTests now writes the elapsed time in each test in the final report
  - faster late binding process for our variants custom types (i.e.
    TSynTableVariantType and TSQLDBRowVariantType): you can call
    SynRegisterCustomVariantType() function to register any other custom
    variant type, and enhance GetProperty/SetProperty process speed
  - includes our optimized RecordCopy procedure in replacement to the slower
    default System.@CopyRecord internal RTL function
  - our optimized Move() and FillChar() will replac the default System RTL
    function, for Delphi versions prior to 2007 (which didn't contain those)
  - new AnsiCharToUTF8(), StringToWinAnsi(), WideStringToWinAnsi(),
    WideStringToUTF8(), CSVOfValue(), IdemPCharArray(), FindUnicode(),
    UpperCaseUnicode(), LowerCaseUnicode() and Split() functions
  - faster GetInt64() function
  - Iso8601ToSecondsPUTF8Char() now returns 0 in case of unexpected format
  - fixed issue in StrCurr64() low-level conversion routine
  - fixed issue in Utf8DecodeToRawUnicodeUI() function
  - new TSynTableFieldProperties.OrderedIndexRefresh method, to allow access
    on OrderedIndex[] even if the index needs to be refreshed
  - new TDynArrayHashed.AddAndMakeUniqueName() method and Hash[] property
  - new TRawByteStringStream class (a TStream using a RawByteString as internal
    storage), especially useful since Delphi 2009
  - new TSynNameValue object, to efficiently handle Name/Value RawUTF8 pairs
    (using hashing for Name search)
  - TTextWriter.CreateOwnedStream now create an internal TRawByteStringStream
    instance for faster process and direct retrieval in the Text method
  - JSONEncode*() global functions will use an internal TRawByteStringStream
    instead of a supplied TMemoryStream
  - new FormatUTF8() overloaded function, handling both '%' and '?' parameters
    (inserting '?' as inlined :(...): parameters, with proper string quote) -
    with associated regression tests

  Version 1.16
  - introducing new TSynAnsiConvert and TSynAnsiFixedWidth classes, able to
    process Unicode to/from Ansi conversion in all possible code pages, with
    generic access methods and optimized handling of fixed width encodings
  - added dedicated Exception classes (ESynException, ETableDataException)
  - TSynLog allows read sharing of the .log created file
  - TSynLog now stores the executable build time, and library name (if any) -
    this is a small change of the .log format as expected by the LogViewer tool
    (so you should upgrade your LogViewer.exe to its latest version)
  - TSynLog and TSynMapFile now handle libraries (.dll/.ocx/.bpl) .map/.mab
    debugging information (only .exe was previously handled)
  - TSynCache now handles an optional Tag: PtrInt value parameter (used e.g.
    to store the row counts of a SQL result cache in mORMot)
  - TSynCache now uses the generic TSynNameValue object from its internal
    hashed list implementation (avoid duplicated code)
  - TMemoryMapText class (and therefore TSynLogFile) is now able to map/open
    an existing file: it will allow e.g. the SynLogViewer to browse a .log file
    which is actually still opened and working by the main application
  - faster RawUnicodeToUtf8() and UTF8ToWideChar() functions, thanks to very
    clever speed-up proposals by Sha (also included in TSynAnsi* classes)
  - JSONDecode() overloaded functions now accept parameter names without case
    sensibility (and a new HandleValuesAsObjectOrArray parameter)
  - new JSONDecode() overloaded function, to properly handle unserialization
    of a JSON object within a buffer (used e.g. for TDynArrayJSONCustomReader)
  - JSON functions now handle '0' as number according to http://json.org specs
  - new TTextWriter.AddJSONEscape() overloaded function, to be used to directly
    serialize some name/value pairs as a JSON object content (used e.g. for
    TDynArrayJSONCustomWriter callbacks)
  - new FileSize(), RoundTo2Digits() and RawByteStringArrayConcat() functions
  - new TPrecisionTimer Pause and Resume methods
  - new TSynTestCase.CheckFailed method (most of the time, Check is sufficient)
  - new TSynLogFamily.IncludeComputerNameInFileName property
  - new TTextWriter.WrRecord method for direct adding of a Base-64 record content
  - new TTextWriter.AddNoJSONEscapeString method
  - new TRawUTF8ListHashed class, with extend TRawUTF8List by using an internal
    hash table to optimized IndexOf() method call (including case sensitivity)
  - new ToVarInt64() and FromVarInt64() functions to encode and decode
    variable-length signed Int64 values (with the corresponding new tftVarInt64
    kind of variable-length column in TSynTableFieldType enumeration)
  - new GotoNextJSONObjectOrArray() and RawUTF8ArrayToQuotedCSV() functions
  - new ReadStringFromStream() and WriteStringToStream() functions
  - fixed some compilation warnings with Delphi XE and XE2
  - fixed issue in TDynArrayHashed if you do not use the external Count
  - fixed potential GPF in TDynArrayHashed.ReHash after TDynArray.Clear call
  - fixed issue in TSynTableFieldProperties.SaveTo about saving wrong indexes
  - fixed issue TSynTableStatement when only one column was retrieved
  - fixed rounding issue in ExtendedToString() and all corresponding wrappers
    like DoubleToStr*, Add(Double...)
  - fixed issue in Hash32() implementation (potential GPF when reading ahead
    by DWORD - get rid of unnecessary asm optimization)
  - fixed issues in function IsJSONString() which returned TRUE for '-' or '+',
    or false positives in some border-line cases (due to wrong uppercase guess):
    now this function is split into IsString() and IsStringJSON() functions
    to explicitely handle null/false/true constant recognition
  - fixed potential false positives of null/false/true in function GetJSONField
  - get rid of wrong "Decimal" parameter in float to text conversion
  - TFileBufferWriter.Create now accepts up to 4 MB internal buffer size
  - increased TDynArrayHashed number of void entries (for speed)
  - modified TDynArray.SaveToStream/LoadFromStream to read or save the data
    from the current stream position
  - fixed GPF in TDynArray.SaveTo in case of invalid internal record layout
  - modified StreamUnSynLZ() so that Source stream will point after all read data
  - TDynArray.SaveToStream() method can now save to any TStream class
  - added TTextWriter.RegisterCustomJSONSerializer() method to allow JSON
    serialization of any dynamic array content (used by TDynArray.LoadFromJSON
    and TTextWriter.AddDynArrayJSON) and record content (used by RecordLoadJSON
    and TTextWriter.AddRecordJSON)
  - added USEPACKAGES conditional to help compiling the unit within packages
  - added optional DOPATCHTRTL to patch the RTL (RecordCopy low-level function)
    only if needed (not patched by default, for compatibility reasons) - you
    may want to use our Enhanced RTL patchs instead for a whole better response
  - new function BinToBase64URI()
  - circumvent some bugs of Delphi XE2 background compiler (main compiler is OK)
  - add premilinary Windows 8 operating system detection (as wEight/wEightServer)

  Version 1.17
  - check of QueryPerformanceFrequency failure, and rollback to low-resolution timer
  - handle properly old .synlz layout (reading compatibility was broken)
  - added TObjectListHashed class, which behaves like TList/TObjectList, but
    will use hashing for (much) faster IndexOf() method, and associated
    TObjectListPropertyHashed class, which allows hashing of a sub-property
    of an object (including some changes made to TDynArray/TDynArrayHashed)
  - new TTextWriter.AddDateTime() overloaded method able to quote the output
  - new TTextWriter.AddFloatStr() method handling partial floating-point text
  - both TTextWriter.AddDateTime() overloaded methods will store '' when value
    is 0, or a pure ISO-8601 date or time if the value is defined as such,
    just as expected by http://www.sqlite.org/lang_datefunc.html - it will also
    reduce average generated JSON/text content size
  - fixed issue about BLOB unproperly serialized into JSON (e.g. now uses null)
  - fixed ticket [e5ad3684b2] about some .map files parsing in TSynMapFile
  - TSynLog stack tracing uses low-level RtlCaptureStackBackTrace() API on CPU64  
  - changed the non expanded JSON format to use lowercase first column names:
    {"fieldCount":1,"values":["col1"... instead of {"FieldCount":1,"Values":[..
  - new SetInt64() procedure for direct assignment of the result
  - TSynTableStatement class now accepts '_' in table and column identifiers
  - fixed implementation issue in function FindNextUTF8WordBegin()
  - fixed false negative issue in TSynSoundEx.UTF8 and TSynSoundEx.Ansi
  - fixed wrong UTF-8 encoding of U+FFF0 used for JSON_BASE64_MAGIC 
  - added an optional parameter to StrToCurr64() function, able to return
    a true Int64 value if no decimal is supplied within the input text buffer
  - enhanced TSynAnsiFixedWidth.UnicodeBufferToAnsi average process speed
  - TSynCache.Reset now returns a boolean stating if something was flushed
  - new SynUnicodeToUtf8(), StringToSynUnicode(), SynUnicodeToString() functions
  - new StrToCurrency() wrapper function
  - new UTF8UpperCopy() and UTF8UpperCopy255() optimized functions
  - new GotoNextNotSpace() and GotoEndOfQuotedString() functions
  - new TMemoryMap.Map() method expecting a file name as parameter
  - new TRawUTF8List.LoadFromFile() method
  - added TDynArray.Equals() method to compare two arrays efficiently
  - added TDynArray and TDynArrayHashed InitSpecific() method able to specify
    how (hashing and) comparison should be processed for a given record
    (includes also some TDynArray/TDynArrayHashed refactoring and optimization)
  - new TObjectHash abstract class to use hashing to find an object
  - TTextWriter.AddJSONEscape() method speed up
  - moved logging threadvars and associated structures into hidden internal
    declaration, for better work with packages (avoid W1032 warning)
  - now JSON parser will handle #1..' ' chars as whitespace (not only ' ')
  - now JSON parser will allow whitespace inserted between any pair of tokens,
    even after true/false/null, as expected by the specification
  - fixed potential Integer Overflow error in Iso8601ToDateTimePUTF8Char*()
  - added PatchCode() and RedirectCodeRestore() procedures, and some code
    refactoring about process in-memory code patching
  - internal FillChar() will now use faster SSE2 instructions on supported CPUs

}


{$I Synopse.inc} // define HASINLINE USETYPEINFO CPU32 CPU64 OWNNORMTOUPPER

{$ifndef LVCL}
{$ifndef DELPHI5OROLDER}
   /// the following conditional will create a custom variant type to
  // access some TSynTable property value
  {$define USESYNTABLEVARIANT}
{$endif}
{$endif}

interface

uses
{$ifdef MSWINDOWS}
  WinApi.Windows,
  WinApi.Messages,
{$endif}
{$ifdef LINUX} // for Kylix
  Libc,
  Types,
{$endif}
  System.Classes,
{$ifndef LVCL}
  Contnrs, // for TObjectList
{$endif}
{$ifdef USESYNTABLEVARIANT}
  Variants,
{$endif}
  SynLz, // needed for TSynMapFile .mab format
  System.SysUtils;


const
  {/ the corresponding version of the freeware Synopse framework }
  SYNOPSE_FRAMEWORK_VERSION = '1.17'{$ifdef LVCL}+' LVCL'{$endif};

{ ************ some custom Exception classes }

type
  /// generic parent class of all custom Exception types of this unit
  ESynException = class(Exception);

  /// exception raised by all TSynTable related code
  ETableDataException = class(Exception);
  

{ ************ common types used for compatibility between compilers and CPU }

{$ifdef FPC} { make cross-compiler and cross-CPU types available to Delphi }
type
  PBoolean = ^Boolean;

{$else}

type
  /// a CPU-dependent unsigned integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
{$ifdef ISDELPHI2009}
  PtrUInt = cardinal; { see http://synopse.info/forum/viewtopic.php?id=136 }
{$else}
  PtrUInt = {$ifdef UNICODE}NativeUInt{$else}cardinal{$endif};
{$endif}
  /// a CPU-dependent unsigned integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrUInt = ^PtrUInt;

  /// a CPU-dependent signed integer type cast of a pointer / register
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PtrInt = {$ifdef UNICODE}NativeInt{$else}integer{$endif};
  /// a CPU-dependent signed integer type cast of a pointer of pointer
  // - used for 64 bits compatibility, native under Free Pascal Compiler
  PPtrInt = ^PtrInt;

  /// unsigned Int64 doesn't exist under older Delphi, but is defined in FPC
  QWord = {$ifdef UNICODE}UInt64{$else}Int64{$endif};
{$endif}

{$ifdef DELPHI5OROLDER}
  // Delphi 5 doesn't have those base types defined :(
const
  varInt64 = $0014; { vt_i8 }

type
  PPointer = ^Pointer;
  PPAnsiChar = ^PAnsiChar;
  PInteger = ^Integer;
  PCardinal = ^Cardinal;
  PWord = ^Word;
  PByte = ^Byte;
  PBoolean = ^Boolean;
  PComp = ^Comp;
  IntegerArray  = array[0..$effffff] of Integer;
  PIntegerArray = ^IntegerArray;
  PointerArray  = array[0..$effffff] of Pointer;
  PPointerArray = ^PointerArray;
  THandle = LongWord;
  PVarData = ^TVarData;
  TVarData = packed record
    // mostly used for varNull, varInt64, varDouble, varString and varAny
    VType: word;
    case Integer of
      0: (Reserved1: Word;
          case Integer of
            0: (Reserved2, Reserved3: Word;
                case Integer of
                  varSmallInt: (VSmallInt: SmallInt);
                  varInteger:  (VInteger: Integer);
                  varSingle:   (VSingle: Single);
                  varDouble:   (VDouble: Double);     // DOUBLE
                  varCurrency: (VCurrency: Currency);
                  varDate:     (VDate: TDateTime);
                  varOleStr:   (VOleStr: PWideChar);
                  varDispatch: (VDispatch: Pointer);
                  varError:    (VError: HRESULT);
                  varBoolean:  (VBoolean: WordBool);
                  varUnknown:  (VUnknown: Pointer);
                  varByte:     (VByte: Byte);
                  varInt64:    (VInt64: Int64);      // INTEGER
                  varString:   (VString: Pointer);   // TEXT
                  varAny:      (VAny: Pointer);
                  varArray:    (VArray: PVarArray);
                  varByRef:    (VPointer: Pointer);
               );
            1: (VLongs: array[0..2] of LongInt); );
  end;
{$endif}

const
  /// internal Code Page for Unicode encoding
  // - used e.g. for Delphi 2009+ UnicodeString=String type
  CP_UTF16 = 1200;

type
  /// a pointer to a PtrUInt array
  TPtrUIntArray = array[0..MaxInt div SizeOf(PtrUInt)-1] of PtrUInt;
  PPtrUIntArray = ^TPtrUIntArray;

  /// a dynamic array of PtrUInt values
  TPtrUIntDynArray = array of PtrUInt;

  /// a dynamic array of TVarData values
  TVarDataDynArray = array of TVarData;

  {/ RawUnicode is an Unicode String stored in an AnsiString
    - faster than WideString, which are allocated in Global heap (for COM)
    - an AnsiChar(#0) is added at the end, for having a true WideChar(#0) at ending
    - length(RawUnicode) returns memory bytes count: use (length(RawUnicode) shr 1)
     for WideChar count (that's why the definition of this type since Delphi 2009
     is AnsiString(1200) and not UnicodeString)
    - pointer(RawUnicode) is compatible with Win32 'Wide' API call
    - mimic Delphi 2009 UnicodeString, without the WideString or Ansi conversion overhead
    - all conversion to/from AnsiString or RawUTF8 must be explicit }
{$ifdef UNICODE} RawUnicode = type AnsiString(CP_UTF16); // Codepage for an UnicodeString
{$else}          RawUnicode = type AnsiString;
{$endif}

  {/ RawUTF8 is an UTF-8 String stored in an AnsiString
    - use this type instead of System.UTF8String, which behavior changed
     between Delphi 2009 compiler and previous versions: our implementation
     is consistent and compatible with all versions of Delphi compiler
    - mimic Delphi 2009 UTF8String, without the charset conversion overhead
    - all conversion to/from AnsiString or RawUnicode must be explicit }
{$ifdef UNICODE} RawUTF8 = type AnsiString(CP_UTF8); // Codepage for an UTF8 string
{$else}          RawUTF8 = type AnsiString; {$endif}

  {/ WinAnsiString is a WinAnsi-encoded AnsiString (code page 1252)
    - use this type instead of System.String, which behavior changed
     between Delphi 2009 compiler and previous versions: our implementation
     is consistent and compatible with all versions of Delphi compiler
    - all conversion to/from RawUTF8 or RawUnicode must be explicit }
{$ifdef UNICODE} WinAnsiString = type AnsiString(1252); // WinAnsi Codepage
{$else}          WinAnsiString = type AnsiString; {$endif}

{$ifndef UNICODE}
  /// define RawByteString, as it does exist in Delphi 2009+
  // - to be used for byte storage into an AnsiString
  // - use this type if you don't want the Delphi compiler not to do any
  // code page conversions when you assign a typed AnsiString to a RawByteString,
  // i.e. a RawUTF8 or a WinAnsiString
  RawByteString = AnsiString;
  /// pointer to a RawByteString
  PRawByteString = ^RawByteString;
{$endif}

  {/ SynUnicode is the fastest available Unicode native string type, depending
    on the compiler used
   - this type is native to the compiler, so you can use Length() Copy() and
     such functions with it (this is not possible with RawUnicodeString type)
   - before Delphi 2009+, it uses slow OLE compatible WideString
     (with our Enhanced RTL, WideString allocation can be made faster by using
     an internal caching mechanism of allocation buffers - WideString allocation
     has been made much faster since Windows Vista/Seven)
   - starting with Delphi 2009, it uses fastest UnicodeString type, which
     allow Copy On Write, Reference Counting and fast heap memory allocation }
  {$ifdef UNICODE}SynUnicode = UnicodeString;
  {$else}         SynUnicode = WideString; {$endif}

  PRawUnicode = ^RawUnicode;
  PRawUTF8 = ^RawUTF8;
  PWinAnsiString = ^WinAnsiString;
  PWinAnsiChar = type PAnsiChar;

  TSetOfAnsiChar = set of AnsiChar;

  /// a simple wrapper to UTF-8 encoded zero-terminated PAnsiChar
  // - PAnsiChar is used only for Win-Ansi encoded text
  // - the Synopse SQLite3 framework uses mostly this PUTF8Char type,
  // because all data is internaly stored and expected to be UTF-8 encoded
  PUTF8Char = type PAnsiChar;
  PPUTF8Char = ^PUTF8Char;

  /// a Row/Col array of PUTF8Char, for containing sqlite3_get_table() result
  TPUtf8CharArray = array[0..MaxInt div SizeOf(PUTF8Char)-1] of PUTF8Char;
  PPUtf8CharArray = ^TPUtf8CharArray;

  /// a pointer to a PAnsiChar array
  TPAnsiCharArray = array[0..MaxInt div SizeOf(PAnsiChar)-1] of PAnsiChar;
  PPAnsiCharArray = ^TPAnsiCharArray;

  /// a dynamic array of PUTF8Char pointers
  TPUtf8CharDynArray = array of PUTF8Char;

  /// a pointer to a RawUTF8 array
  TRawUTF8Array = array[0..MaxInt div SizeOf(RawUTF8)-1] of RawUTF8;
  PRawUTF8Array = ^TRawUTF8Array;

  /// a dynamic array of UTF-8 encoded strings
  TRawUTF8DynArray = array of RawUTF8;
  PRawUTF8DynArray = ^TRawUTF8DynArray;

  /// a dynamic array of WinAnsi encoded strings
  TWinAnsiDynArray = array of WinAnsiString;
  PWinAnsiDynArray = ^TWinAnsiDynArray;

  /// a dynamic array of RawByteString
  TRawByteStringDynArray = array of RawByteString;

  /// a dynamic array of generic VCL strings
  TStringDynArray = array of string;
  PStringDynArray = ^TStringDynArray;

  /// a dynamic array of TDateTime values
  TDateTimeDynArray = array of TDateTime;
  PDateTimeDynArray = ^TDateTimeDynArray;

  /// a dynamic array of WideString values
  TWideStringDynArray = array of WideString;
  PWideStringDynArray = ^TWideStringDynArray;

  /// a dynamic array of SynUnicode values
  TSynUnicodeDynArray = array of SynUnicode;
  PSynUnicodeDynArray = ^TSynUnicodeDynArray;

  PIntegerDynArray = ^TIntegerDynArray;
  TIntegerDynArray = array of integer;
  PCardinalDynArray = ^TCardinalDynArray;
  TCardinalDynArray = array of cardinal;
  PInt64DynArray = ^TInt64DynArray;
  TInt64DynArray = array of Int64;
  PDoubleDynArray = ^TDoubleDynArray;
  TDoubleDynArray = array of double;
  PCurrencyDynArray = ^TCurrencyDynArray;
  TCurrencyDynArray = array of Currency;
  TWordDynArray = array of word;
  PWordDynArray = ^TWordDynArray;
  TByteDynArray = array of byte;
  PByteDynArray = ^TByteDynArray;
  TObjectDynArray = array of TObject;
  PObjectDynArray = ^TObjectDynArray;

  TWordArray  = array[0..MaxInt div SizeOf(word)-1] of word;
  PWordArray = ^TWordArray;

  TInt64Array = array[0..MaxInt div SizeOf(Int64)-1] of Int64;
  PInt64Array = ^TInt64Array;

  TCardinalArray = array[0..MaxInt div SizeOf(cardinal)-1] of cardinal;
  PCardinalArray = ^TCardinalArray;

  TSmallIntArray = array[0..MaxInt div SizeOf(SmallInt)-1] of SmallInt;
  PSmallIntArray = ^TSmallIntArray;

  TDoubleArray = array[0..MaxInt div SizeOf(double)-1] of double;
  PDoubleArray = ^TDoubleArray;

  TRawByteStringArray = array[0..MaxInt div SizeOf(RawByteString)-1] of RawByteString;
  PRawByteStringArray = ^TRawByteStringArray;

  TPtrIntArray = array[0..MaxInt div SizeOf(PtrInt)-1] of PtrInt;
  PPtrIntArray = ^TPtrIntArray;

  PInt64Rec = ^Int64Rec;
  {$ifndef DELPHI5OROLDER}
  PIInterface = ^IInterface;
  {$endif}


{ ************ fast UTF-8 / Unicode / Ansi types and conversion routines }

type
  /// an abstract class to handle Ansi to/from Unicode translation
  // - implementations of this class will handle efficiently all Code Pages
  // - this default implementation will use the Operating System APIs
  // - you should not create your own class instance by yourself, but should
  // better retrieve an instance using TSynAnsiConvert.Engine(), which will
  // initialize either a TSynAnsiFixedWidth or a TSynAnsiConvert instance on need
  TSynAnsiConvert = class
  protected
    fCodePage: Integer;
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: integer); reintroduce; virtual;
    /// returns the engine corresponding to a given code page
    // - a global list of TSynAnsiConvert instances is handled by the unit -
    // therefore, caller should not release the returned instance
    // - will return nil in case of unhandled code page
    class function Engine(aCodePage: integer): TSynAnsiConvert;
    /// direct conversion of a PAnsiChar buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    // - this default implementation will use the Operating System APIs
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar; SourceChars: Cardinal): PWideChar; overload; virtual;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - a #0 char is appended at the end (and result will point to it)
    // - this default implementation will use the Operating System APIs
    function AnsiBufferToUTF8(Dest: PUTF8Char; Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char; overload; virtual;
    /// convert any Ansi Text into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode; overload;
    /// convert any Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(Source: PAnsiChar; SourceChars: Cardinal): RawUnicode; overload; virtual;
    /// convert any Ansi buffer into an Unicode String
    // - returns a SynUnicode, i.e. Delphi 2009+ UnicodeString or a WideString
    function AnsiToUnicodeString(Source: PAnsiChar; SourceChars: Cardinal): SynUnicode; 
    /// convert any Ansi Text into an UTF-8 encoded String
    // - internaly calls AnsiBufferToUTF8 virtual method
    function AnsiToUTF8(const AnsiText: RawByteString): RawUTF8;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded string
    // - will call AnsiBufferToUnicode() overloaded virtual method
    function AnsiBufferToRawUTF8(Source: PAnsiChar; SourceChars: Cardinal): RawUTF8; overload;
    /// direct conversion of an Unicode buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - this default implementation will rely on the Operating System for
    // all non ASCII-7 chars
    function UnicodeBufferToAnsi(Dest: PAnsiChar; Source: PWideChar; SourceChars: Cardinal): PAnsiChar; overload; virtual;
    /// direct conversion of an Unicode buffer into an Ansi Text
    function UnicodeBufferToAnsi(Source: PWideChar; SourceChars: Cardinal): RawByteString; overload;
    /// convert any Unicode-encoded String into Ansi Text
    // - internaly calls UnicodeBufferToAnsi virtual method
    function RawUnicodeToAnsi(const Source: RawUnicode): RawByteString;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    function UTF8BufferToAnsi(Dest: PAnsiChar; Source: PUTF8Char; SourceChars: Cardinal): PAnsiChar; overload; virtual; 
    /// convert any UTF-8 encoded buffer into Ansi Text
    // - internaly calls UTF8BufferToAnsi virtual method
    function UTF8BufferToAnsi(Source: PUTF8Char; SourceChars: Cardinal): RawByteString; overload;
    /// convert any UTF-8 encoded String into Ansi Text
    // - internaly calls UTF8BufferToAnsi virtual method
    function UTF8ToAnsi(const UTF8: RawUTF8): RawByteString;
    /// convert any Ansi Text (providing a From converted) into Ansi Text
    function AnsiToAnsi(From: TSynAnsiConvert; const Source: RawByteString): RawByteString; overload; 
    /// convert any Ansi buffer (providing a From converted) into Ansi Text
    function AnsiToAnsi(From: TSynAnsiConvert; Source: PAnsiChar; SourceChars: cardinal): RawByteString; overload;
    /// corresponding code page
    property CodePage: Integer read fCodePage;
  end;

  /// a class to handle Ansi to/from Unicode translation of fixed width encoding
  // (i.e. non MBCS)
  // - this class will handle efficiently all Code Page availables without MBCS
  // encoding - like WinAnsi (1252) or Russian (1251)
  // - it will use internal fast look-up tables for such encodings
  // - this class could take some time to generate, and will consume more than
  // 64 KB of memory: you should not create your own class instance by yourself,
  // but should better retrieve an instance using TSynAnsiConvert.Engine(), which
  // will initialize either a TSynAnsiFixedWidth or a TSynAnsiConvert instance
  // on need
  // - this class has some additional methods (e.g. IsValid*) which take
  // advantage of the internal lookup tables to provide some fast process
  TSynAnsiFixedWidth = class(TSynAnsiConvert)
  protected
    fAnsiToWide: TWordDynArray;
    fWideToAnsi: TByteDynArray;
  public
    /// initialize the internal conversion engine
    constructor Create(aCodePage: integer); override;
    /// direct conversion of a PAnsiChar buffer into an Unicode buffer
    // - Dest^ buffer must be reserved with at least SourceChars*2 bytes
    function AnsiBufferToUnicode(Dest: PWideChar; Source: PAnsiChar; SourceChars: Cardinal): PWideChar; override;
    /// direct conversion of a PAnsiChar buffer into a UTF-8 encoded buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - a #0 char is appended at the end (and result will point to it)
    function AnsiBufferToUTF8(Dest: PUTF8Char; Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char; override;
    /// convert any Ansi buffer into an Unicode String
    // - returns a value using our RawUnicode kind of string
    function AnsiToRawUnicode(Source: PAnsiChar; SourceChars: Cardinal): RawUnicode; override;
    /// direct conversion of an Unicode buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars*3 bytes
    // - this overriden version will use internal lookup tables for fast process
    function UnicodeBufferToAnsi(Dest: PAnsiChar; Source: PWideChar; SourceChars: Cardinal): PAnsiChar; override;
    /// direct conversion of an UTF-8 encoded buffer into a PAnsiChar buffer
    // - Dest^ buffer must be reserved with at least SourceChars bytes
    function UTF8BufferToAnsi(Dest: PAnsiChar; Source: PUTF8Char; SourceChars: Cardinal): PAnsiChar; override;
    /// conversion of a wide char into the corresponding Ansi character 
    // - return -1 for an unknown WideChar in the current code page 
    function WideCharToAnsiChar(wc: cardinal): integer;
    /// return TRUE if the supplied unicode buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsi(WideText: PWideChar; Length: integer): boolean; overload;
    /// return TRUE if the supplied unicode buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsi(WideText: PWideChar): boolean; overload;
    /// return TRUE if the supplied UTF-8 buffer only contains characters of
    // the corresponding Ansi code page
    // - i.e. if the text can be displayed using this code page
    function IsValidAnsiU(UTF8Text: PUTF8Char): boolean;
    /// return TRUE if the supplied UTF-8 buffer only contains 8 bits characters
    // of the corresponding Ansi code page
    // - i.e. if the text can be displayed with only 8 bit unicode characters
    // (e.g. no "tm" or such) within this code page
    function IsValidAnsiU8Bit(UTF8Text: PUTF8Char): boolean;
    /// direct access to the Ansi-To-Unicode lookup table
    // - use this array like AnsiToWide: array[byte] of word
    property AnsiToWide: TWordDynArray read fAnsiToWide;
    /// direct access to the Unicode-To-Ansi lookup table
    // - use this array like WideToAnsi: array[word] of byte
    // - any unhandled WideChar will return ord('?')
    property WideToAnsi: TByteDynArray read fWideToAnsi;
  end;


var
  /// global TSynAnsiConvert instance to handle WinAnsi encoding (code page 1252)
  // - this instance is global and instantied during the whole program life time
  WinAnsiConvert: TSynAnsiFixedWidth;

  /// global TSynAnsiConvert instance to handle current system encoding
  // - this is the encoding as used by the AnsiString Delphi, so will be used
  // before Delphi 2009 to speed-up VCL string handling (especially for UTF-8)
  // - this instance is global and instantied during the whole program life time
  CurrentAnsiConvert: TSynAnsiConvert;


const
  /// MIME content type used for JSON communication (as used by the Microsoft
  // WCF framework and the YUI framework)
  JSON_CONTENT_TYPE = 'application/json; charset=UTF-8';

  /// MIME content type used for plain UTF-8 text
  TEXT_CONTENT_TYPE = 'text/plain; charset="UTF-8"';

  /// HTTP header, as defined in the corresponding RFC
  HEADER_CONTENT_TYPE = 'Content-Type: ';

  /// HTTP header for MIME content type used for plain UTF-8 text 
  TEXT_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE+TEXT_CONTENT_TYPE;

  /// MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE = 'text/html; charset="UTF-8"';

  /// HTTP header for MIME content type used for UTF-8 encoded HTML
  HTML_CONTENT_TYPE_HEADER = HEADER_CONTENT_TYPE+HTML_CONTENT_TYPE;

  /// US English Windows Code Page, i.e. WinAnsi standard character encoding
  CODEPAGE_US = 1252;
  CODEPAGE_RU = 1251;


/// conversion of a wide char into a WinAnsi (CodePage 1252) char
// - return '?' for an unknown WideChar in code page 1252
function WideCharToWinAnsiChar(wc: cardinal): AnsiChar;
  {$ifdef HASINLINE}inline;{$endif}

/// conversion of a wide char into a WinAnsi (CodePage 1252) char index
// - return -1 for an unknown WideChar in code page 1252
function WideCharToWinAnsi(wc: cardinal): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied buffer only contains 7-bits Ansi characters
function IsAnsiCompatible(PC: PAnsiChar): boolean; overload;

/// return TRUE if the supplied buffer only contains 7-bits Ansi characters
function IsAnsiCompatible(PC: PAnsiChar; Len: integer): boolean; overload;

/// return TRUE if the supplied buffer only contains 7-bits Ansi characters
function IsAnsiCompatible(PW: PWideChar): boolean; overload;

/// return TRUE if the supplied text only contains 7-bits Ansi characters
function IsAnsiCompatible(const Text: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied buffer only contains 7-bits Ansi characters
function IsAnsiCompatible(PW: PWideChar; Len: integer): boolean; overload;

/// return TRUE if the supplied unicode buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsi(WideText: PWideChar): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied unicode buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsi(WideText: PWideChar; Length: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied UTF-8 buffer only contains WinAnsi characters
// - i.e. if the text can be displayed using ANSI_CHARSET
function IsWinAnsiU(UTF8Text: PUTF8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// return TRUE if the supplied UTF-8 buffer only contains WinAnsi 8 bit characters
// - i.e. if the text can be displayed using ANSI_CHARSET with only 8 bit unicode
// characters (e.g. no "tm" or such)
function IsWinAnsiU8Bit(UTF8Text: PUTF8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// UTF-8 encode one Unicode character into Dest
// - return the number of bytes written into Dest (i.e. 1,2 or 3) 
function UnicodeCharToUtf8(Dest: PUTF8Char; aWideChar: PtrUInt): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a UTF-8 encoded String
// - faster than SysUtils: don't use Utf8Encode(WideString) -> no Windows.Global(),
// and use a fixed pre-calculated array for individual chars conversion
function WinAnsiToUtf8(const S: WinAnsiString): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a UTF-8 encoded String
// - faster than SysUtils: don't use Utf8Encode(WideString) -> no Windows.Global(),
// and use a fixed pre-calculated array for individual chars conversion
function WinAnsiToUtf8(WinAnsi: PAnsiChar; WinAnsiLen: integer): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi PAnsiChar buffer into a UTF-8 encoded buffer
// - Dest^ buffer must be reserved with at least SourceChars*3
function WinAnsiBufferToUtf8(Dest: PUTF8Char; Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a WinAnsi (CodePage 1252) string into a Unicode encoded String
// - very fast, by using a fixed pre-calculated array for individual chars conversion
function WinAnsiToRawUnicode(const S: WinAnsiString): RawUnicode;

/// direct conversion of a WinAnsi (CodePage 1252) string into a Unicode buffer
// - very fast, by using a fixed pre-calculated array for individual chars conversion
// - text will be truncated if necessary to avoid buffer overflow in Dest[]
procedure WinAnsiToUnicodeBuffer(const S: WinAnsiString; Dest: PWordArray; DestLen: integer);
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded string into a WinAnsi String
function Utf8ToWinAnsi(const S: RawUTF8): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded zero terminated buffer into a WinAnsi String
function Utf8ToWinAnsi(P: PUTF8Char): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded zero terminated buffer into a RawUTF8 String
procedure Utf8ToRawUTF8(P: PUTF8Char; var result: RawUTF8);
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded buffer into a WinAnsi PAnsiChar buffer
function UTF8ToWinPChar(dest: PAnsiChar; source: PUTF8Char; count: integer): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a UTF-8 encoded buffer into a WinAnsi shortstring buffer
procedure UTF8ToShortString(var dest: shortstring; source: PUTF8Char);

/// convert an UTF-8 encoded text into a WideChar array
// - faster than System.UTF8ToUnicode
// - sourceBytes can by 0, therefore length is computed from zero terminated source
// - enough place must be available in dest
// - a WideChar(#0) is added at the end (if something is written)
// - returns the BYTE count written in dest, excluding the ending WideChar(#0)
function UTF8ToWideChar(dest: pWideChar; source: PUTF8Char; sourceBytes: PtrInt=0): PtrInt; overload;

/// convert an UTF-8 encoded text into a WideChar array
// - faster than System.UTF8ToUnicode
// - this overloaded function expect a MaxDestChars parameter
// - sourceBytes can not be 0 for this function
// - enough place must be available in dest
// - a WideChar(#0) is added at the end (if something is written)
// - returns the byte count written in dest, excluding the ending WideChar(#0)
function UTF8ToWideChar(dest: pWideChar; source: PUTF8Char; MaxDestChars, sourceBytes: PtrInt): PtrInt; overload;

/// calculate the Unicode character count (i.e. the glyph count),
// UTF-8 encoded in source^
// - faster than System.UTF8ToUnicode with dest=nil
function Utf8ToUnicodeLength(source: PUTF8Char): PtrUInt;

/// calculate the character count of the first line UTF-8 encoded in source^
// - end the count at first #13 or #10 character
function Utf8FirstLineToUnicodeLength(source: PUTF8Char): PtrInt;

/// convert a UTF-8 encoded buffer into a RawUnicode string
// - if L is 0, L is computed from zero terminated P buffer
// - RawUnicode is ended by a WideChar(#0)
// - faster than System.Utf8Decode() which uses slow widestrings
function Utf8DecodeToRawUnicode(P: PUTF8Char; L: integer): RawUnicode; overload;

/// convert a UTF-8 string into a RawUnicode string
function Utf8DecodeToRawUnicode(const S: RawUTF8): RawUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a UTF-8 string into a RawUnicode string
// - this version doesn't resize the length of the result RawUnicode
// and is therefore usefull before a Win32 Unicode API call (with nCount=-1)
// - if DestLen is not nil, the resulting length (in bytes) will be stored within
function Utf8DecodeToRawUnicodeUI(const S: RawUTF8; DestLen: PInteger=nil): RawUnicode;

/// convert a RawUnicode PWideChar into a UTF-8 string
function RawUnicodeToUtf8(P: PWideChar; WideCharCount: integer): RawUTF8; overload;

/// convert a RawUnicode PWideChar into a UTF-8 buffer
// - replace system.UnicodeToUtf8 implementation, which is rather slow
// since Delphi 2009+
function RawUnicodeToUtf8(Dest: PUTF8Char; DestLen: PtrInt; Source: PWideChar; SourceLen: PtrInt): PtrInt; overload;

/// convert a RawUnicode PWideChar into a UTF-8 string
// - this version doesn't resize the resulting RawUTF8 string, but return
// the new resulting RawUTF8 byte count into UTF8Length
function RawUnicodeToUtf8(P: PWideChar; WideCharCount: integer; out UTF8Length: integer): RawUTF8; overload;

/// convert a RawUnicode string into a UTF-8 string
function RawUnicodeToUtf8(const Unicode: RawUnicode): RawUTF8; overload;

/// convert a SynUnicode string into a UTF-8 string
function SynUnicodeToUtf8(const Unicode: SynUnicode): RawUTF8; 

/// convert a WideString into a UTF-8 string
function WideStringToUTF8(const aText: WideString): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

/// direct conversion of a Unicode encoded buffer into a WinAnsi PAnsiChar buffer
procedure RawUnicodeToWinPChar(dest: PAnsiChar; source: PWideChar; WideCharCount: integer);
  {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUnicode PWideChar into a WinAnsi (code page 1252) string
function RawUnicodeToWinAnsi(P: PWideChar; WideCharCount: integer): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a RawUnicode string into a WinAnsi (code page 1252) string
function RawUnicodeToWinAnsi(const Unicode: RawUnicode): WinAnsiString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a WideString into a WinAnsi (code page 1252) string
function WideStringToWinAnsi(const Wide: WideString): WinAnsiString;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an AnsiChar buffer (of a given code page) into a UTF-8 string
procedure AnsiCharToUTF8(P: PAnsiChar; L: Integer; var result: RawUTF8; ACP: integer);

/// convert any Raw Unicode encoded String into a generic SynUnicode Text
function RawUnicodeToSynUnicode(const Unicode: RawUnicode): SynUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert any Raw Unicode encoded String into a generic SynUnicode Text
function RawUnicodeToSynUnicode(P: PWideChar; WideCharCount: integer): SynUnicode; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an Unicode buffer into a WinAnsi (code page 1252) string
procedure UnicodeBufferToWinAnsi(source: PWideChar; out Dest: WinAnsiString);

/// convert an Unicode buffer into a generic VCL string
function UnicodeBufferToString(source: PWideChar): string;

{$ifdef UNICODE}
/// convert a Delphi 2009+ Unicode string into our UTF-8 string
function UnicodeStringToUtf8(const S: string): RawUTF8; inline;

/// convert a Delphi 2009+ Unicode string into a WinAnsi (code page 1252) string
function UnicodeStringToWinAnsi(const S: string): WinAnsiString; inline;

// this function is the same as direct RawUTF8=AnsiString(CP_UTF8) assignment
// but is faster, since it uses no Win32 API call
function UTF8DecodeToUnicodeString(const S: RawUTF8): UnicodeString; overload; inline;

/// convert our UTF-8 encoded buffer into a Delphi 2009+ Unicode string
// - this function is the same as direct assignment, since RawUTF8=AnsiString(CP_UTF8),
// but is faster, since use no Win32 API call
function UTF8DecodeToUnicodeString(P: PUTF8Char; L: integer): UnicodeString; overload;

/// convert a Win-Ansi encoded buffer into a Delphi 2009+ Unicode string
// - this function is faster than default RTL, since use no Win32 API call
function WinAnsiToUnicodeString(WinAnsi: PAnsiChar; WinAnsiLen: integer): UnicodeString;
{$endif}

/// convert any generic VCL Text into an UTF-8 encoded String
// - it's prefered to use TLanguageFile.StringToUTF8() method in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToUTF8(const Text: string): RawUTF8;
  {$ifdef HASINLINE}inline;{$endif}

{$ifndef LVCL}
/// convert any Variant into UTF-8 encoded String
// - will use an internal SynUnicode temporary conversion
function VariantToUTF8(const V: Variant): RawUTF8;
{$endif}

/// convert any generic VCL Text buffer into an UTF-8 encoded buffer
// - Dest must be able to receive at least SourceChars*3 bytes
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringBufferToUtf8(Dest: PUTF8Char; Source: PChar; SourceChars: PtrInt): PUTF8Char;

/// convert any generic VCL Text into a Raw Unicode encoded String
// - it's prefered to use TLanguageFile.StringToUTF8() method in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToRawUnicode(const S: string): RawUnicode; overload;

/// convert any generic VCL Text into a SynUnicode encoded String
// - it's prefered to use TLanguageFile.StringToUTF8() method in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToSynUnicode(const S: string): SynUnicode; 

/// convert any generic VCL Text into a Raw Unicode encoded String
// - it's prefered to use TLanguageFile.StringToUTF8() method in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function StringToRawUnicode(P: PChar; L: integer): RawUnicode; overload;

/// convert any Raw Unicode encoded string into a generic VCL Text
function RawUnicodeToString(const U: RawUnicode): string; overload;

/// convert any Raw Unicode encoded buffer into a generic VCL Text
function RawUnicodeToString(P: PWideChar; L: integer): string; overload;

/// convert any Raw Unicode encoded buffer into a generic VCL Text
procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string); overload;

/// convert any SynUnicode encoded string into a generic VCL Text
function SynUnicodeToString(const U: SynUnicode): string; 
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic VCL Text
// - it's prefered to use TLanguageFile.UTF8ToString() in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function UTF8ToString(const Text: RawUTF8): string; 
  {$ifdef HASINLINE}inline;{$endif}

/// convert any UTF-8 encoded buffer into a generic VCL Text
// - it's prefered to use TLanguageFile.UTF8ToString() in SQLite3i18n,
// which will handle full i18n of your application
// - it will work as is with Delphi 2009+ (direct unicode conversion)
// - under older version of Delphi (no unicode), it will use the
// current RTL codepage, as with WideString conversion (but without slow
// WideString usage)
function UTF8DecodeToString(P: PUTF8Char; L: integer): string;
  {$ifdef UNICODE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic WideString Text
function UTF8ToWideString(const Text: RawUTF8): WideString; overload;
  {$ifdef UNICODE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic WideString Text
procedure UTF8ToWideString(const Text: RawUTF8; var result: WideString); overload;
  {$ifdef UNICODE}inline;{$endif}

/// convert any UTF-8 encoded String into a generic WideString Text
procedure UTF8ToWideString(Text: PUTF8Char; Len: integer; var result: WideString); overload;

/// convert any UTF-8 encoded String into a generic SynUnicode Text
function UTF8ToSynUnicode(const Text: RawUTF8): SynUnicode; overload;

/// convert any UTF-8 encoded String into a generic SynUnicode Text
procedure UTF8ToSynUnicode(const Text: RawUTF8; var result: SynUnicode); overload;

/// convert any Ansi 7 bit encoded String into a generic VCL Text
// - the Text content must contain only 7 bit pure ASCII characters
function Ansi7ToString(const Text: RawByteString): string; overload;

/// convert any Ansi 7 bit encoded String into a generic VCL Text
// - the Text content must contain only 7 bit pure ASCII characters
function Ansi7ToString(Text: PWinAnsiChar; Len: integer): string; overload;

/// convert any generic VCL Text into Ansi 7 bit encoded String
// - the Text content must contain only 7 bit pure ASCII characters
function StringToAnsi7(const Text: string): RawByteString;

/// convert any generic VCL Text into WinAnsi (Win-1252) 8 bit encoded String
function StringToWinAnsi(const Text: string): WinAnsiString;

/// fast Format() function replacement, optimized for RawUTF8
// - only supported token is %, which will be inlined in the resulting string
// according to each Args[] supplied item
// - resulting string has no length limit and uses fast concatenation
// - maximum count of supplied argument in Args is 12
// - note that cardinal values should be type-casted to Int64() (otherwise
// the integer mapped value will be transmitted, therefore wrongly)
function FormatUTF8(Format: PUTF8Char; const Args: array of const): RawUTF8; overload;

/// fast Format() function replacement, handling % and ? parameters
// - will include Args[] for every % in Format
// - will inline Params[] for every ? in Format, handling special "inlined"
// parameters, as exected by SQLite3Commons, i.e. :(1234): for numerical
// values, and :('quoted '' string'): for textual values
// - resulting string has no length limit and uses fast concatenation
// - maximum count of supplied argument in Args is 12
// - note that cardinal values should be type-casted to Int64() (otherwise
// the integer mapped value will be transmitted, therefore wrongly)
function FormatUTF8(Format: PUTF8Char; const Args, Params: array of const): RawUTF8; overload;

/// convert an open array (const Args: array of const) argument to an UTF-8
// encoded text
// - note that cardinal values should be type-casted to Int64() (otherwise
// the integer mapped value will be transmitted, therefore wrongly)
procedure VarRecToUTF8(const V: TVarRec; var result: RawUTF8);

/// fast concatenation of several AnsiStrings
function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;

type
  /// function prototype used internally for UTF-8 buffer comparaison
  // - used in SQLite3commons during TSQLTable rows sort and by TSQLQuery
  TUTF8Compare = function(P1,P2: PUTF8Char): PtrInt;

{$ifndef ISDELPHI2007ANDUP}
type
  TBytes = array of byte;
{$endif}

{$ifndef ENHANCEDRTL} { is our Enhanced Runtime (or LVCL) library not installed? }

{$define OWNNORMTOUPPER} { NormToUpper[] exists only in our enhanced RTL }

{$ifndef PUREPASCAL} { these functions are implemented in asm }
{$ifndef LVCL} { don't define these functions twice }

{$ifndef PUREPASCAL}

/// faster implementation of Move() for Delphi versions with no FastCode inside
// - Delphi RTL will be patched in memory to run this faster version
procedure FillChar(var Dest; Count: Integer; Value: Byte);

{$ifndef ISDELPHI2007ANDUP}
/// faster implementation of Move() for Delphi versions with no FastCode inside
// - Delphi RTL will be patched in memory to run this faster version
procedure Move(const Source; var Dest; Count: Integer);
{$endif}
{$endif}

{$ifndef FPC}  { these asm function use some low-level system.pas calls }

/// use our fast asm RawUTF8 version of Trim()
function Trim(const S: RawUTF8): RawUTF8;

/// use our fast asm version of CompareMem()
function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;

{$endif FPC}

{$ifdef UNICODE}
/// our fast RawUTF8 version of Pos(), for Unicode only compiler
// - this Pos() is seldom used, but this RawUTF8 specific version is needed
// by Delphi 2009+, to avoid two unnecessary conversions into UnicodeString
function Pos(const substr, str: RawUTF8): Integer; overload; inline;
{$endif}

{$endif LVCL}
{$endif PUREPASCAL}

{$endif ENHANCEDRTL}


/// use our fast RawUTF8 version of IntToStr()
// - without any slow UnicodeString=String->AnsiString conversion for Delphi 2009
// - only usefull if our Enhanced Runtime (or LVCL) library is not installed
function Int64ToUtf8(Value: Int64): RawUTF8;

/// use our fast RawUTF8 version of IntToStr()
// - without any slow UnicodeString=String->AnsiString conversion for Delphi 2009
// - only usefull if our Enhanced Runtime (or LVCL) library is not installed
function Int32ToUtf8(Value: integer): RawUTF8;

/// optimized conversion of a cardinal into RawUTF8
function UInt32ToUtf8(Value: cardinal): RawUTF8;

/// faster version than default SysUtils.IntToStr implementation
function IntToString(Value: integer): string; overload;

/// faster version than default SysUtils.IntToStr implementation
function IntToString(Value: cardinal): string; overload;

/// faster version than default SysUtils.IntToStr implementation
function IntToString(Value: Int64): string; overload;

/// convert a floating-point value to its numerical text equivalency
function DoubleToString(Value: Double): string;

/// convert a currency value from its Int64 binary representation into
// its numerical text equivalency
function Curr64ToString(Value: Int64): string;

const
  /// best possible precision when rendering a "double" kind of float
  // - can be used as parameter for ExtendedToString/ExtendedToStr
  DOUBLE_PRECISION = 15;

/// convert a floating-point value to its numerical text equivalency
// - returns the count of chars stored into S (S[0] is not set)
function ExtendedToString(var S: ShortString; Value: Extended; Precision: integer): integer;

/// convert a floating-point value to its numerical text equivalency
function ExtendedToStr(Value: Extended; Precision: integer): RawUTF8;

/// convert a floating-point value to its numerical text equivalency
function DoubleToStr(Value: Double): RawUTF8;

/// fast retrieve the position of a given character
function PosChar(Str: PUTF8Char; Chr: AnsiChar): PUTF8Char;

/// a non case-sensitive RawUTF8 version of Pos()
// - substr is expected to be already in upper case
// - this version handle only 7 bit ASCII (no accentuated characters)
function PosI(substr: PUTF8Char; const str: RawUTF8): Integer;

/// a non case-sensitive RawUTF8 version of Pos()
// - substr is expected to be already in upper case
// - this version will decode the UTF-8 content before using NormToUpper[]
function PosIU(substr: PUTF8Char; const str: RawUTF8): Integer;

{/ internal fast integer val to text conversion
 - expect the last available temporary char position in P
 - return the last written char position (write in reverse order in P^)
 - typical use:
  !function Int32ToUTF8(Value : integer): RawUTF8;
  !var tmp: array[0..15] of AnsiChar;
  !    P: PAnsiChar;
  !begin
  !  P := StrInt32(@tmp[15],Value);
  !  SetString(result,P,@tmp[15]-P);
  !end;
 - not to be called directly: use IntToStr() instead }
function StrInt32(P: PAnsiChar; val: PtrInt): PAnsiChar;

{/ internal fast unsigned integer val to text conversion
 - expect the last available temporary char position in P
 - return the last written char position (write in reverse order in P^) }
function StrUInt32(P: PAnsiChar; val: PtrUInt): PAnsiChar;

{/ internal fast Int64 val to text conversion
   - same calling convention as with StrInt32() above }
function StrInt64(P: PAnsiChar; val: Int64): PAnsiChar;
  {$ifdef CPU64}inline;{$endif}

/// fast add some characters to a RawUTF8 string
// - faster than SetString(tmp,Buffer,BufferLen); Text := Text+tmp;
procedure AppendBufferToRawUTF8(var Text: RawUTF8; Buffer: pointer; BufferLen: PtrInt);

/// fast add some characters from a RawUTF8 string into a given buffer
function AppendRawUTF8ToBuffer(Buffer: PUTF8Char; const Text: RawUTF8): PUTF8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// use our fast version of StrComp(), to be used with PUTF8Char
function StrComp(Str1, Str2: PUTF8Char): PtrInt;

/// use our fast version of StrIComp()
function StrIComp(Str1, Str2: PUTF8Char): PtrInt;

/// our fast version of StrLen(), to be used with PUTF8Char
function StrLen(S: PUTF8Char): PtrInt;

/// our fast version of StrLen(), to be used with PWideChar
function StrLenW(S: PWideChar): PtrInt;

/// use our fast version of StrComp(), to be used with PWideChar
function StrCompW(Str1, Str2: PWideChar): PtrInt;

/// use our fast version of StrCompL(), to be used with PUTF8Char
function StrCompL(P1,P2: PUTF8Char; L, Default: Integer): PtrInt;

/// use our fast version of StrCompIL(), to be used with PUTF8Char
function StrCompIL(P1,P2: PUTF8Char; L, Default: Integer): PtrInt;

{$ifdef USENORMTOUPPER}
{$ifdef OWNNORMTOUPPER}
type
  TNormTable = packed array[AnsiChar] of AnsiChar;
  TNormTableByte = packed array[byte] of byte;

var
  /// the NormToUpper[] array is defined in our Enhanced RTL: define it now
  //  if it was not installed
  NormToUpper: TNormTable;
  NormToUpperByte: TNormTableByte absolute NormToUpper;

  /// the NormToLower[] array is defined in our Enhanced RTL: define it now
  //  if it was not installed
  NormToLower: TNormTable;
  NormToLowerByte: TNormTableByte absolute NormToLower;
{$endif}
{$else}
{$undef OWNNORMTOUPPER}
{$endif}

var
  /// this table will convert 'a'..'z' into 'A'..'Z'
  // - so it will work with UTF-8 without decoding, whereas NormToUpper[] expects
  // WinAnsi encoding
  NormToUpperAnsi7: TNormTable;
  NormToUpperAnsi7Byte: TNormTableByte absolute NormToUpperAnsi7;

/// get the signed 32 bits integer value stored in P^
// - we use the PtrInt result type, even if expected to be 32 bits, to use
// native CPU register size (don't want any 32 bits overflow here)
function GetInteger(P: PUTF8Char): PtrInt; overload;

/// get the signed 32 bits integer value stored in P^
// - this version return 0 in err if no error occured, and 1 if an invalid
// character was found, not its exact index as for the val() function
// - we use the PtrInt result type, even if expected to be 32 bits, to use
// native CPU register size (don't want any 32 bits overflow here)
function GetInteger(P: PUTF8Char; var err: integer): PtrInt; overload;

/// get the unsigned 32 bits integer value stored in P^
// - we use the PtrInt result type, even if expected to be 32 bits, to use
// native CPU register size (don't want any 32 bits overflow here)
function GetCardinal(P: PUTF8Char): PtrUInt;

/// get the unsigned 32 bits integer value stored in P^
// - if P if nil or not start with a valid numerical value, returns Default
function GetCardinalDef(P: PUTF8Char; Default: PtrUInt): PtrUInt;

/// get the unsigned 32 bits integer value stored as Unicode string in P^
function GetCardinalW(P: PWideChar): PtrUInt;

/// get the 64 bits integer value stored in P^
function GetInt64(P: PUTF8Char): Int64; overload;
  {$ifdef CPU64}inline;{$endif}

/// get the 64 bits integer value stored in P^
procedure SetInt64(P: PUTF8Char; var result: Int64); 
  {$ifdef CPU64}inline;{$endif}

/// get the 64 bits integer value stored in P^
// - set the err content to the index of any faulty character, 0 if conversion
// was successful (same as the standard val function)
function GetInt64(P: PUTF8Char; var err: integer): Int64; overload;

/// get the extended floating point value stored in P^
// - set the err content to the index of any faulty character, 0 if conversion
// was successful (same as the standard val function)
function GetExtended(P: PUTF8Char; out err: integer): extended; overload;

/// get the extended floating point value stored in P^
// - this overloaded version returns 0 as a result if the content of P is invalid
function GetExtended(P: PUTF8Char): extended; overload;

/// get the WideChar stored in P^ (decode UTF-8 if necessary)
function GetUTF8Char(P: PUTF8Char): PtrUInt;

/// get the WideChar stored in P^ (decode UTF-8 if necessary) and set new pos to Next
function NextUTF8Char(P: PUTF8Char; out Next: PUTF8Char): PtrUInt;

/// encode a string to be compatible with URI encoding
function UrlEncode(const svar: RawUTF8): RawUTF8; overload;

/// decode a string compatible with URI encoding into its original value
// - you can specify the decoding range (as in copy(s,i,len) function)
function UrlDecode(const s: RawUTF8; i: PtrInt = 1; len: PtrInt = -1): RawUTF8; overload;

/// decode a string compatible with URI encoding into its original value
function UrlDecode(U: PUTF8Char): RawUTF8; overload;

/// decode a specified parameter compatible with URI encoding into its original
// textual value
// - UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@Next)
// will return Next^='where=...' and V='*'
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeValue(U, Upper: PUTF8Char; var Value: RawUTF8; Next: PPUTF8Char=nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// integer numerical value
// - UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInteger(U, Upper: PUTF8Char; var Value: integer; Next: PPUTF8Char=nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// cardinal numerical value
// - UrlDecodeCardinal('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeCardinal(U, Upper: PUTF8Char; var Value: Cardinal; Next: PPUTF8Char=nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// Int64 numerical value
// - UrlDecodeInt64('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
// will return Next^='where=...' and O=20
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeInt64(U, Upper: PUTF8Char; var Value: Int64; Next: PPUTF8Char=nil): boolean;

/// decode a specified parameter compatible with URI encoding into its original
// floating-point value
// - UrlDecodeExtended('price=20.45&where=LastName%3D%27M%C3%B4net%27','PRICE=',P,@Next)
// will return Next^='where=...' and P=20.45
// - if Upper is not found, Value is not modified, and result is FALSE
// - if Upper is found, Value is modified with the supplied content, and result is TRUE
function UrlDecodeExtended(U, Upper: PUTF8Char; var Value: Extended; Next: PPUTF8Char=nil): boolean;

/// returns TRUE if all supplied parameters does exist in the URI encoded text
// - UrlDecodeNeedParameters('price=20.45&where=LastName%3D','PRICE,WHERE') will
// return TRUE
function UrlDecodeNeedParameters(U, CSVUpper: PUTF8Char): boolean;



/// case unsensitive test of P1 and P2 content
// - use it with properties only (A..Z,0..9 chars)
function IdemPropName(const P1,P2: shortstring): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with properties only (A..Z,0..9 chars)
// - this version expect P2 to be a PAnsiChar with a specified length
function IdemPropName(const P1: shortstring; P2: PUTF8Char; P2Len: integer): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// case unsensitive test of P1 and P2 content
// - use it with properties only (A..Z,0..9 chars)
function IdemPropNameU(const P1,P2: RawUTF8): boolean;

/// returns true if the beginning of p^ is the same as up^
// - ignore case - up^ must be already Upper
// - chars are compared as 7 bit Ansi only (no accentuated characters): but when
// you only need to search for field names e.g. IdemPChar() is prefered, because
// it'll be faster than IdemPCharU(), if UTF-8 decoding is not mandatory
function IdemPChar(p, up: PUTF8Char): boolean;

/// returns the index of a matching beginning of p^ in upArray[]
// - returns -1 if no item matched
// - ignore case - up^ must be already Upper
// - chars are compared as 7 bit Ansi only (no accentuated characters): but when
// you only need to search for field names e.g. IdemPChar() is prefered, because
// it'll be faster than IdemPCharU(), if UTF-8 decoding is not mandatory
function IdemPCharArray(p: PUTF8Char; const upArray: array of PUTF8Char): integer;

/// returns true if the beginning of p^ is the same as up^
// - ignore case - up^ must be already Upper
// - this version will decode the UTF-8 content before using NormToUpper[], so
// it will be slower than the IdemPChar() function above, but will handle
// WinAnsi accentuated characters (e.g. 'e' acute will be matched as 'E')
function IdemPCharU(p, up: PUTF8Char): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// returns true if the beginning of p^ is same as up^
// - ignore case - up^ must be already Upper
// - this version expect p^ to point to an Unicode char array
function IdemPCharW(p: pWideChar; up: PUTF8Char): boolean;

/// returns true if the file name extension contained in p^ is the same same as extup^
// - ignore case - extup^ must be already Upper
// - chars are compared as WinAnsi (codepage 1252), not as UTF-8
function IdemFileExt(p, extup: PUTF8Char): Boolean;

/// retrieve the next UCS2 value stored in U, then update the U pointer
// - this function will decode the UTF-8 content before using NormToUpper[]
// - will return '?' if the UCS2 value is higher than #255: so use this function
// only if you need to deal with ASCII characters (e.g. it's used for Soundex
// and for ContainsUTF8 function)
function GetNextUTF8Upper(var U: PUTF8Char): cardinal;  {$ifdef HASINLINE}inline;{$endif}

/// points to the beginning of the next word stored in U
// - returns nil if reached the end of U (i.e. #0 char)
// - here a "word" is a Win-Ansi word, i.e. '0'..'9', 'A'..'Z'
function FindNextUTF8WordBegin(U: PUTF8Char): PUTF8Char;

/// return true if up^ is contained inside the UTF-8 buffer p^
// - search up^ at the beginning of every UTF-8 word (aka in Soundex)
// - here a "word" is a Win-Ansi word, i.e. '0'..'9', 'A'..'Z'
// - up^ must be already Upper
function ContainsUTF8(p, up: PUTF8Char): boolean;

/// copy source into dest^ with 7 bits upper case conversion
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar)
function UpperCopy255(dest: PAnsiChar; const source: RawUTF8): PAnsiChar;

/// copy WideChar source into dest^ with upper case conversion
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar)
function UpperCopy255W(dest: PAnsiChar; const source: SynUnicode): PAnsiChar;

/// copy source into dest^ with 7 bits upper case conversion
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar)
function UpperCopy(dest: PAnsiChar; const source: RawUTF8): PAnsiChar;

/// copy source into dest^ with 7 bits upper case conversion
// - returns final dest pointer
// - this special version expect source to be a shortstring
function UpperCopyShort(dest: PAnsiChar; const source: shortstring): PAnsiChar;

{$ifdef USENORMTOUPPER}
/// fast UTF-8 comparaison using the NormToUpper[] array for all 8 bits values
// - this version expects u1 and u2 to be zero-terminated
// - this version will decode the UTF-8 content before using NormToUpper[]
function UTF8IComp(u1, u2: PUTF8Char): PtrInt;

/// copy WideChar source into dest^ with upper case conversion, using the
// NormToUpper[] array for all 8 bits values, encoding the result as UTF-8
// - returns final dest pointer
function UTF8UpperCopy(Dest, Source: PUTF8Char; SourceChars: Cardinal): PUTF8Char;

/// copy WideChar source into dest^ with upper case conversion, using the
// NormToUpper[] array for all 8 bits values, encoding the result as UTF-8
// - returns final dest pointer
// - will copy up to 255 AnsiChar (expect the dest buffer to be array[byte] of
// AnsiChar), with UTF-8 encoding
function UTF8UpperCopy255(dest: PAnsiChar; const source: RawUTF8): PUTF8Char;
  {$ifdef HASINLINE}inline;{$endif} 

/// fast UTF-8 comparaison using the NormToUpper[] array for all 8 bits values
// - this version expects u1 and u2 not to be necessary zero-terminated, but
// uses L1 and L2 as length for u1 and u2 respectively
// - use this function for SQLite3 collation (TSQLCollateFunc)
// - this version will decode the UTF-8 content before using NormToUpper[]
function UTF8ILComp(u1, u2: PUTF8Char; L1,L2: cardinal): PtrInt;

/// fast case-insensitive Unicode comparaison
// - use the NormToUpperAnsi7Byte[] array, i.e. compare 'a'..'z' as 'A'..'Z'
// - this version expects u1 and u2 to be zero-terminated
function AnsiICompW(u1, u2: PWideChar): PtrInt;

/// SameText() overloaded function with proper UTF-8 decoding
// - fast version using NormToUpper[] array for all Win-Ansi characters
// - this version will decode the UTF-8 content before using NormToUpper[]
function SameTextU(const S1, S2: RawUTF8): Boolean;
  {$ifdef HASINLINE}inline;{$endif} 

/// fast conversion of the supplied text into 8 bit uppercase
// - this will not only convert 'a'..'z' into 'A'..'Z', but also accentuated
// latin characters ('e' acute into 'E' e.g.), using NormToUpper[] array
// - it will decode the supplied UTF-8 content to handle more than
// 7 bit of ascii characters (so this function is dedicated to WinAnsi code page
// 1252 characters set)
function UpperCaseU(const S: RawUTF8): RawUTF8;

/// fast conversion of the supplied text into 8 bit lowercase
// - this will not only convert 'A'..'Z' into 'a'..'z', but also accentuated
// latin characters ('E' acute into 'e' e.g.), using NormToLower[] array
// - it will convert decode the supplied UTF-8 content to handle more than
// 7 bit of ascii characters
function LowerCaseU(const S: RawUTF8): RawUTF8;

/// fast conversion of the supplied text into 8 bit case sensitivity
// - convert the text in-place, returns the resulting length
// - it will decode the supplied UTF-8 content to handle more than 7 bit
// of ascii characters during the conversion (leaving not WinAnsi characters
// untouched)
// - will not set the last char to #0 (caller must do that if necessary)
function ConvertCaseUTF8(P: PUTF8Char; const Table: TNormTableByte): PtrInt;

{$endif}

/// fast conversion of the supplied text into uppercase
// - this will only convert 'a'..'z' into 'A'..'Z' (no NormToUpper use), and
// will therefore by correct with true UTF-8 content, but only for 7 bit
function UpperCase(const S: RawUTF8): RawUTF8;

/// fast conversion of the supplied text into lowercase
// - this will only convert 'A'..'Z' into 'a'..'z' (no NormToLower use), and
// will therefore by correct with true UTF-8 content
function LowerCase(const S: RawUTF8): RawUTF8;

/// accurate conversion of the supplied UTF-8 content into the corresponding
// upper-case Unicode characters
// - this version will use the Operating System API, and will therefore be
// much slower than UpperCase/UpperCaseU versions, but will handle all
// kind of unicode characters
function UpperCaseUnicode(const S: RawUTF8): RawUTF8;

/// accurate conversion of the supplied UTF-8 content into the corresponding
// lower-case Unicode characters
// - this version will use the Operating System API, and will therefore be
// much slower than LowerCase/LowerCaseU versions, but will handle all
// kind of unicode characters
function LowerCaseUnicode(const S: RawUTF8): RawUTF8;

///  trims leading whitespace characters from the string by removing
// new line, space, and tab characters
function TrimLeft(const S: RawUTF8): RawUTF8;

/// trims trailing whitespace characters from the string by removing trailing
// newline, space, and tab characters
function TrimRight(const S: RawUTF8): RawUTF8;

/// fast WinAnsi comparaison using the NormToUpper[] array for all 8 bits values
function AnsiIComp(Str1, Str2: PWinAnsiChar): PtrInt;
  {$ifndef USENORMTOUPPER} {$ifdef PUREPASCAL}
  {$ifdef HASINLINE}inline;{$endif} {$endif} {$endif}

/// extract a line from source array of chars
// - next will contain the beginning of next line, or nil if source if ended
function GetNextLine(source: PUTF8Char; out next: PUTF8Char): RawUTF8;

{$ifdef UNICODE}
/// extract a line from source array of chars
// - next will contain the beginning of next line, or nil if source if ended
// - this special version expect UnicodeString pointers, and return an UnicodeString
function GetNextLineW(source: PWideChar; out next: PWideChar): string;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - this special version expect UnicodeString pointer, and return an UnicodeString
function FindIniNameValueW(P: PWideChar; UpperName: PUTF8Char): string;

/// find a Name= Value in a [Section] of a INI Unicode Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryW(const Content: string; const Section, Name: RawUTF8): string;

{$ifdef FPC}
/// our fast RawUTF8 version of Trim(), for FPC only
function Trim(const S: RawUTF8): RawUTF8;
{$endif}

{$ifdef PUREPASCAL}

/// our fast RawUTF8 version of Trim(), for Unicode only compiler
// - this Trim() is seldom used, but this RawUTF8 specific version is needed
// by Delphi 2009+, to avoid two unnecessary conversions into UnicodeString
function Trim(const S: RawUTF8): RawUTF8;

{$ifndef UNICODE}
/// our fast RawUTF8 version of Pos(), for Unicode only compiler
// - this Pos() is seldom used, but this RawUTF8 specific version is needed
// by Delphi 2009+, to avoid two unnecessary conversions into UnicodeString
function Pos(const substr, str: RawUTF8): Integer; overload; inline;
{$endif UNICODE}

{$endif PUREPASCAL}

{$endif UNICODE}

/// faster RawUTF8 Equivalent of standard StrUtils.PosEx
function PosEx(const SubStr, S: RawUTF8; Offset: Cardinal = 1): Integer;

/// split a RawUTF8 string into two strings, according to SepStr separator
// - if SepStr is not found, LeftStr=Str and RightStr=''
// - if ToUpperCase is TRUE, then LeftStr and RightStr will be made uppercase
procedure Split(const Str, SepStr: RawUTF8; var LeftStr, RightStr: RawUTF8; ToUpperCase: boolean=false);

/// fast replacement of StringReplace(S, OldPattern, NewPattern,[rfReplaceAll]);
function StringReplaceAll(const S, OldPattern, NewPattern: RawUTF8): RawUTF8;

/// fast replace of a specified char into a given string
function StringReplaceChars(const Source: RawUTF8; OldChar, NewChar: AnsiChar): RawUTF8;

{/ format a text content with quotes
 - UTF-8 version of the function available in SysUtils
 - this function implements what is specified in the official SQLite3
   documentation: "A string constant is formed by enclosing the string in single
   quotes ('). A single quote within the string can be encoded by putting two
   single quotes in a row - as in Pascal." }
function QuotedStr(const S: RawUTF8; Quote: AnsiChar=''''): RawUTF8; overload;
  {$ifdef HASINLINE}inline;{$endif}

 {/ format a buffered text content with quotes
 - this function implements what is specified in the official SQLite3
   documentation: "A string constant is formed by enclosing the string in single
   quotes ('). A single quote within the string can be encoded by putting two
   single quotes in a row - as in Pascal." }
function QuotedStr(Text: PUTF8Char; Quote: AnsiChar): RawUTF8; overload;

/// unquote a SQL-compatible string
// - the first character in P^ must be either ', either " then double quotes
// are transformed into single quotes
// - 'text '' end'   -> text ' end
// - "text "" end"   -> text " end
// - returns nil if P doesn't contain a valid SQL string
// - returns a pointer just after the quoted text otherwise
function UnQuoteSQLString(P: PUTF8Char; out Value: RawUTF8): PUTF8Char;

/// get the next character after a quoted buffer
// - the first character in P^ must be either ', either "
function GotoEndOfQuotedString(P: PUTF8Char): PUTF8Char;

/// get the next character not in [#1..' ']
function GotoNextNotSpace(P: PUTF8Char): PUTF8Char;
  {$ifdef HASINLINE}inline;{$endif}

/// go to the beginning of the SQL statement, ignoring all blanks and comments
// - used to check the SQL statement command (e.g. is it a SELECT?)
function SQLBegin(P: PUTF8Char): PUTF8Char;

/// return true if the parameter is void or begin with a 'SELECT' SQL statement
// - used to avoid code injection and to check if the cache must be flushed
// - 'VACUUM' statement also returns true, since doesn't change the data content
function isSelect(P: PUTF8Char): boolean;

/// return true if IdemPChar(source,search), and go to the next line of source
function IdemPCharAndGetNextLine(var source: PUTF8Char; search: PUTF8Char): boolean;

/// return line begin from source array of chars, and go to next line
// - next will contain the beginning of next line, or nil if source if ended
function GetNextLineBegin(source: PUTF8Char; out next: PUTF8Char): PUTF8Char;

/// compute the line length from source array of chars
// - end counting at either #0, #13 or #10
function GetLineSize(P,PEnd: PUTF8Char): PtrUInt;

/// returns true if the line length from source array of chars is not less than
// the specified count
function GetLineSizeSmallerThan(P,PEnd: PUTF8Char; aMinimalCount: integer): boolean;

/// return next CSV string from P, nil if no more
function GetNextItem(var P: PUTF8Char; Sep: AnsiChar= ','): RawUTF8;

/// return next CSV string from P, nil if no more
// - this function returns the generic string type of the compiler, and
// therefore can be used with ready to be displayed text (e.g. for the VCL)
function GetNextItemString(var P: PChar; Sep: Char= ','): string;

/// return next string delimited with #13#10 from P, nil if no more
// - this function returns RawUnicode string type
function GetNextStringLineToRawUnicode(var P: PChar): RawUnicode;

/// append some text lines with the supplied Values[]
// - if any Values[] item is '', no line is added
// - otherwize, appends 'Caption: Value', with Caption taken from CSV
procedure AppendCSVValues(const CSV: string; const Values: array of string;
  var Result: string; const AppendBefore: string=#13#10);

/// return a CSV list of the iterated same value
// - e.g. CSVOfValue('?',3)='?,?,?'
function CSVOfValue(const Value: RawUTF8; Count: cardinal; const Sep: RawUTF8=','): RawUTF8;

 /// retrieve the next CSV separated bit index
// - each bit was stored as BitIndex+1, i.e. 0 to mark end of CSV chunk
procedure SetBitCSV(var Bits; BitsCount: integer; var P: PUTF8Char);

/// convert a set of bit into a CSV content
// - each bit is stored as BitIndex+1
// - ',0' is always appended at the end of the CSV chunk to mark its end
function GetBitCSV(const Bits; BitsCount: integer): RawUTF8;

/// return next CSV string from P, nil if no more
procedure GetNextItemShortString(var P: PUTF8Char; out Dest: ShortString; Sep: AnsiChar= ',');

/// return next CSV string as unsigned integer from P, 0 if no more
function GetNextItemCardinal(var P: PUTF8Char; Sep: AnsiChar= ','): PtrUInt;

/// return next CSV string as unsigned integer from P, 0 if no more
// - this version expect P^ to point to an Unicode char array
function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar= ','): PtrUInt;

/// return next CSV string as double from P, 0.0 if no more
function GetNextItemDouble(var P: PUTF8Char; Sep: AnsiChar= ','): double;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
function GetCSVItem(P: PUTF8Char; Index: PtrUInt; Sep: AnsiChar = ','): RawUTF8;

/// return n-th indexed CSV string in P, starting at Index=0 for first one
// - this function return the generic string type of the compiler, and
// therefore can be used with ready to be displayed text (i.e. the VCL)
function GetCSVItemString(P: PChar; Index: PtrUInt; Sep: Char = ','): string;

/// return the index of a Value in a CSV string
// - start at Index=0 for first one
// - return -1 if specified Value was not found in CSV items
function FindCSVIndex(CSV: PUTF8Char; const Value: RawUTF8; Sep: AnsiChar = ',';
  CaseSensitive: boolean=true): integer;

/// add the strings in the specified CSV text into a dynamic array of UTF-8 strings
procedure CSVToRawUTF8DynArray(CSV: PUTF8Char; var Result: TRawUTF8DynArray;
  Sep: AnsiChar = ',');

/// return the corresponding CSV text from a dynamic array of UTF-8 strings
function RawUTF8ArrayToCSV(const Values: array of RawUTF8; const Sep: RawUTF8= ','): RawUTF8;

/// return the corresponding CSV quoted text from a dynamic array of UTF-8 strings
// - apply QuoteStr() function to each Values[] item
function RawUTF8ArrayToQuotedCSV(const Values: array of RawUTF8; const Sep: RawUTF8=',';
  Quote: AnsiChar=''''): RawUTF8;

/// append some prefix to all CSV values
// ! AddPrefixToCSV('One,Two,Three','Pre')='PreOne,PreTwo,PreThree'
function AddPrefixToCSV(CSV: PUTF8Char; const Prefix: RawUTF8;
  Sep: AnsiChar = ','): RawUTF8;

/// return the index of Value in Values[], -1 if not found
function FindRawUTF8(const Values: TRawUTF8DynArray; const Value: RawUTF8;
  CaseSensitive: boolean=true): integer;

/// true if Value was added successfully in Values[]
function AddRawUTF8(var Values: TRawUTF8DynArray; const Value: RawUTF8;
  NoDuplicates: boolean=false; CaseSensitive: boolean=true): boolean;

/// true if both TRawUTF8DynArray are the same
// - comparison is case-sensitive
function RawUTF8DynArrayEquals(const A,B: TRawUTF8DynArray): boolean;

/// find a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntry(const Content, Section,Name: RawUTF8): RawUTF8;

/// find a Name= Value in a [Section] of a INI WinAnsi Content
// - same as FindIniEntry(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniEntry(const Content, Section,Name: RawUTF8): RawUTF8;

/// find a Name= numeric Value in a [Section] of a INI RawUTF8 Content and
// return it as an integer, or 0 if not found
// - this function scans the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', find the Name= value before any [Section]
function FindIniEntryInteger(const Content, Section,Name: RawUTF8): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// find a Name= Value in a [Section] of a .INI file
// - if Section equals '', find the Name= value before any [Section]
// - use internaly fast FindIniEntry() function above
function FindIniEntryFile(const FileName: TFileName; const Section,Name: RawUTF8): RawUTF8;

/// update a Name= Value in a [Section] of a INI RawUTF8 Content
// - this function scans and update the Content memory buffer, and is
// therefore very fast (no temporary TMemIniFile is created)
// - if Section equals '', update the Name= value before any [Section]
procedure UpdateIniEntry(var Content: RawUTF8; const Section,Name,Value: RawUTF8);

/// update a Name= Value in a [Section] of a .INI file
// - if Section equals '', update the Name= value before any [Section]
// - use internaly fast UpdateIniEntry() function above
procedure UpdateIniEntryFile(const FileName: TFileName; const Section,Name,Value: RawUTF8);

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
function FindSectionFirstLine(var source: PUTF8Char; search: PUTF8Char): boolean;

/// find the position of the [SEARCH] section in source
// - return true if [SEARCH] was found, and store pointer to the line after it in source
// - this version expect source^ to point to an Unicode char array
function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;

/// retrieve the whole content of a section as a string
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8; overload;

/// retrieve the whole content of a section as a string
// - use SectionFirstLine() then previous GetSectionContent()
function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// delete a whole [Section]
// - if EraseSectionHeader is TRUE (default), then the [Section] line is also
// deleted together with its content lines
// - return TRUE if something was changed in Content
// - return FALSE if [Section] doesn't exist or is already void
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean=true): boolean; overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
procedure ReplaceSection(var Content: RawUTF8; const SectionName,
  NewSectionContent: RawUTF8); overload;

/// replace a whole [Section] content by a new content
// - create a new [Section] if none was existing
// - SectionFirstLine may have been obtained by FindSectionFirstLine() function above
procedure ReplaceSection(SectionFirstLine: PUTF8Char;
  var Content: RawUTF8; const NewSectionContent: RawUTF8); overload;

/// return TRUE if Value of UpperName does exist in P, till end of current section
// - expect UpperName as 'NAME='
function ExistsIniName(P: PUTF8Char; UpperName: PUTF8Char): boolean;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
function FindIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): RawUTF8;

/// return TRUE if the Value of UpperName exists in P, till end of current section
// - expect UpperName as 'NAME='
function ExistsIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): boolean;

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - same as FindIniNameValue(), but the value is converted from WinAnsi into UTF-8
function FindWinAnsiIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): RawUTF8;

/// find the integer Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
// - return 0 if no NAME= entry was found
function FindIniNameValueInteger(P: PUTF8Char; UpperName: PUTF8Char): integer;
  {$ifdef HASINLINE}inline;{$endif}

/// read a File content into a String
// - content can be binary or text
// - returns '' if file was not found or any read error occured
// - uses RawByteString for byte storage, thatever the codepage is
function StringFromFile(const FileName: TFileName): RawByteString;

/// create a File from a string content
// - uses RawByteString for byte storage, thatever the codepage is
function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean=false): boolean;

/// read an UTF-8 text from a TStream
// - format is Length(Integer):Text, i.e. the one used by WriteStringToStream
// - will return '' if there is no such text in the stream
// - you can set a MaxAllowedSize value, if you know how long the size should be
function ReadStringFromStream(S: TStream; MaxAllowedSize: integer=255): RawUTF8;

/// write an UTF-8 text into a TStream
// - format is Length(Integer):Text, i.e. the one used by ReadStringFromStream
procedure WriteStringToStream(S: TStream; const Text: RawUTF8);

/// get the file date and time
// - returns 0 if file doesn't exist
function FileAgeToDateTime(const FileName: TFileName): TDateTime;

/// get the file size
// - returns 0 if file doesn't exist
function FileSize(const FileName: TFileName): Int64;

{$ifdef DELPHI5OROLDER}
/// DirectoryExists returns a boolean value that indicates whether the
//  specified directory exists (and is actually a directory)
function DirectoryExists(const Directory: string): Boolean;

/// retrieve the full path name of the given execution module (e.g. library)
function GetModuleName(Module: HMODULE): TFileName;

/// try to encode a time
function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
{$endif}

/// extract file name, without its extension
function GetFileNameWithoutExt(const FileName: TFileName): TFileName;

/// extract a file extension from a file name, then compare with a comma
// separated list of extensions
// - e.g. GetFileNameExtIndex('test.log','exe,log,map')=1
// - will return -1 if no file extension match
// - will return any matching extension, starting count at 0
// - extension match is case-insensitive
function GetFileNameExtIndex(const FileName, CSVExt: TFileName): integer;

/// retrieve a property value in a text-encoded class
// - follows the Delphi serialized text object format, not standard .ini
// - if the property is a string, the simple quotes ' are trimed
function FindObjectEntry(const Content, Name: RawUTF8): RawUTF8;

/// retrieve a filename property value in a text-encoded class
// - follows the Delphi serialized text object format, not standard .ini
// - if the property is a string, the simple quotes ' are trimed
// - any file path and any extension are trimmed
function FindObjectEntryWithoutExt(const Content, Name: RawUTF8): RawUTF8;


type
  {/ available pronunciations for our fast Soundex implementation }
  TSynSoundExPronunciation =
    (sndxEnglish, sndxFrench, sndxSpanish, sndxNone);

  TSoundExValues = array[0..ord('Z')-ord('B')] of byte;
  PSoundExValues = ^TSoundExValues;

  PSynSoundEx = ^TSynSoundEx;
  {/ fast search of a text value, using the Soundex searching mechanism
    - Soundex is a phonetic algorithm for indexing names by sound,
      as pronounced in a given language. The goal is for homophones to be
      encoded to the same representation so that they can be matched despite
      minor differences in spelling
    - this implementation is very fast and can be used e.g. to parse and search
      in a huge text buffer
    - This version also handles french and spanish pronunciations on request,
      which differs from default Soundex, i.e. English }
  TSynSoundEx = {$ifndef UNICODE}object{$else}record{$endif}
  private
    Search, FirstChar: cardinal;
    fValues: PSoundExValues;
  public
    /// prepare for a Soundex search
    // - you can specify another language pronunciation than default english
    function Prepare(UpperValue: PAnsiChar;
      Lang: TSynSoundExPronunciation=sndxEnglish): boolean;
    /// return true if prepared value is contained in a text buffer
    // (UTF-8 encoded), by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in U^
    function UTF8(U: PUTF8Char): boolean;
    /// return true if prepared value is contained in a ANSI text buffer
    // by using the SoundEx comparison algorithm
    // - search prepared value at every word beginning in A^
    function Ansi(A: PAnsiChar): boolean;
  end;

{/ Retrieve the Soundex value of a text word, from Ansi buffer
  - Return the soundex value as an easy to use cardinal value, 0 if the
    incoming string contains no valid word
  - if next is defined, its value is set to the end of the encoded word
    (so that you can call again this function to encode a full sentence) }
function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar=nil;
  Lang: TSynSoundExPronunciation=sndxEnglish): cardinal;

{/ Retrieve the Soundex value of a text word, from UTF-8 buffer
  - Return the soundex value as an easy to use cardinal value, 0 if the
    incoming string contains no valid word
  - if next is defined, its value is set to the end of the encoded word
    (so that you can call again this function to encode a full sentence)
  - very fast: all UTF-8 decoding is handled on the fly }
function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char=nil;
  Lang: TSynSoundExPronunciation=sndxEnglish): cardinal;

const
  /// number of bits to use for each interresting soundex char
  // - default is to use 8 bits, i.e. 4 soundex chars, which is the
  // standard approach
  // - for a more detailled soundex, use 4 bits resolution, which will
  // compute up to 7 soundex chars in a cardinal (that's our choice)
  SOUNDEX_BITS = 4;

/// return true if UpperValue (Ansi) is contained in A^ (Ansi)
// - find UpperValue starting at word beginning, not inside words
function FindAnsi(A, UpperValue: PAnsiChar): boolean;

/// return true if UpperValue (Ansi) is contained in U^ (UTF-8 encoded)
// - find UpperValue starting at word beginning, not inside words
// - UTF-8 decoding is done on the fly (no temporary decoding buffer is used)
function FindUTF8(U: PUTF8Char; UpperValue: PAnsiChar): boolean;

/// return true if Uppe (Unicode encoded) is contained in U^ (UTF-8 encoded)
// - will use the slow but accurate Operating System API to perform the
// comparison at Unicode-level
function FindUnicode(PW: PWideChar; Upper: PWideChar; UpperLen: integer): boolean;

/// trim first lowercase chars ('otDone' will return 'Done' e.g.)
// - return an RawUTF8 string: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
function TrimLeftLowerCase(V: PShortString): RawUTF8;

/// convert a CamelCase string into a space separated one
// - 'OnLine' will return 'On line' e.g., and 'OnMyLINE' will return 'On my LINE'
// - '_' char is transformed into ' - '
// - '__' chars are transformed into ': '
// - return an RawUTF8 string: enumeration names are pure 7bit ANSI with Delphi 7
// to 2007, and UTF-8 encoded with Delphi 2009+
function UnCamelCase(const S: RawUTF8): RawUTF8; overload;

/// convert a CamelCase string into a space separated one
// - 'OnLine' will return 'On line' e.g., and 'OnMyLINE' will return 'On my LINE'
// - return the char count written into D^
// - D^ and P^ are expected to be UTF-8 encoded: enumeration and property names
// are pure 7bit ANSI with Delphi 7 to 2007, and UTF-8 encoded with Delphi 2009+
// - '_' char is transformed into ' - '
// - '__' chars are transformed into ': '
function UnCamelCase(D, P: PUTF8Char): integer; overload;

/// UnCamelCase and translate a char buffer
// - P is expected to be #0 ended
// - return "string" type, i.e. UnicodeString for Delphi 2009+
procedure GetCaptionFromPCharLen(P: PUTF8Char; out result: string);

/// will get a class name as UTF-8
// - will trim 'T', 'TSyn', 'TSQL' or 'TSQLRecord' left side of the class name
// - will encode the class name as UTF-8 (for Unicode Delphi versions)
// - is used e.g. to extract the SQL table name for a TSQLRecord class
function GetDisplayNameFromClass(C: TClass): RawUTF8;

///  UnCamelCase and translate the class name, triming any left 'T', 'TSyn',
// 'TSQL' or 'TSQLRecord'
// - return generic VCL string type, i.e. UnicodeString for Delphi 2009+
function GetCaptionFromClass(C: TClass): string;

/// UnCamelCase and translate the enumeration item
function GetCaptionFromEnum(aTypeInfo: pointer; aIndex: integer): string;

/// convert a char set to a code page
function CharSetToCodePage(CharSet: integer): cardinal;

/// convert a code page to a char set
function CodePageToCharSet(CodePage: Cardinal): Integer;

/// retrieve the MIME content type from a supplied binary buffer
// - return the MIME type, ready to be appended to a 'Content-Type: ' HTTP header
// - default is 'application/octet-stream' or 'application/extension' if
// FileName was specified
// - see @http://en.wikipedia.org/wiki/Internet_media_type for most common values
function GetMimeContentType(Content: Pointer; Len: integer;
   const FileName: TFileName=''): RawUTF8;

/// retrieve if some content is compressed, from a supplied binary buffer
// - returns TRUE, if the header in binary buffer "may" be compressed (this method
// can trigger false positives), e.g. begin with zip/gz/gif/wma/png/jpeg markers
function IsContentCompressed(Content: Pointer; Len: integer): boolean;

/// retrieve the index where to insert a PUTF8Char in a sorted PUTF8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive (so will work with any PAnsiChar)
// - returns -1 if the specified Value was found (i.e. adding will duplicate a value)
function FastLocatePUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char): PtrInt; overload;

/// retrieve the index where to insert a PUTF8Char in a sorted PUTF8Char array
// - this overloaded function accept a custom comparison function for sorting
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive (so will work with any PAnsiChar)
// - returns -1 if the specified Value was found (i.e. adding will duplicate a value)
function FastLocatePUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char;
  Compare: TUTF8Compare): PtrInt; overload;

/// retrieve the index where is located a PUTF8Char in a sorted PUTF8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive (so will work with any PAnsiChar)
// - returns -1 if the specified Value was not found
function FastFindPUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char): PtrInt; overload;

/// retrieve the index where is located a PUTF8Char in a sorted PUTF8Char array
// - R is the last index of available entries in P^ (i.e. Count-1)
// - string comparison is case-sensitive (so will work with any PAnsiChar)
// - returns -1 if the specified Value was not found
function FastFindPUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char;
  Compare: TUTF8Compare): PtrInt; overload;

/// add a RawUTF8 value in an alphaticaly sorted dynamic array of RawUTF8
// - returns the index where the Value was added successfully in Values[]
// - returns -1 if the specified Value was alredy present in Values[]
//  (we must avoid any duplicate for binary search)
// - if CoValues is set, its content will be moved to allow inserting a new
// value at CoValues[result] position - a typical usage of CoValues is to store
// the corresponding ID to each RawUTF8 item
// - if FastLocatePUTF8CharSorted() has been already called, this index can
// be set to optional ForceIndex parameter
// - by default, exact (case-sensitive) match is used; you can specify a custom
// compare function if needed in Compare optional parameter
function AddSortedRawUTF8(var Values: TRawUTF8DynArray; var ValuesCount: integer;
  const Value: RawUTF8; CoValues: PIntegerDynArray=nil; ForcedIndex: PtrInt=-1;
  Compare: TUTF8Compare=nil): PtrInt;

/// delete a RawUTF8 item in a dynamic array of RawUTF8
// - if CoValues is set, the integer item at the same index is also deleted
function DeleteRawUTF8(var Values: TRawUTF8DynArray; var ValuesCount: integer;
  Index: integer; CoValues: PIntegerDynArray=nil): boolean;

/// sort a dynamic array of RawUTF8 items
// - if CoValues is set, the integer items are also synchronized
// - by default, exact (case-sensitive) match is used; you can specify a custom
// compare function if needed in Compare optional parameter
procedure QuickSortRawUTF8(var Values: TRawUTF8DynArray; ValuesCount: integer;
  CoValues: PIntegerDynArray=nil; Compare: TUTF8Compare=nil);

/// fast search of an unsigned integer position in an integer array
// - Count is the number of cardinal entries in P^
// - returns P where P^=Value
// - returns nil if Value was not found
function IntegerScan(P: PCardinalArray; Count: PtrInt; Value: cardinal): PCardinal;

/// fast search of an unsigned integer position in an integer array
// - returns true if P^=Value within Count entries
// - returns false if Value was not found
function IntegerScanExists(P: PCardinalArray; Count: PtrInt; Value: cardinal): boolean;

/// fast search of an unsigned integer position in an integer array
// - Count is the number of integer entries in P^
// - return index of P^[index]=Value
// - return -1 if Value was not found
function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;

/// sort an Integer array, low values first
procedure QuickSortInteger(ID: PIntegerArray; L, R: PtrInt);

/// copy an integer array, then sort it, low values first
procedure CopyAndSortInteger(Values: PIntegerArray; ValuesCount: integer;
  var Dest: TIntegerDynArray);  

/// fast binary search of an integer value in a sorted integer array
// - R is the last index of available integer entries in P^ (i.e. Count-1)
// - return index of P^[index]=Value
// - return -1 if Value was not found
function FastFindIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;

/// retrieve the index where to insert an integer value in a sorted integer array
// - R is the last index of available integer entries in P^ (i.e. Count-1)
// - returns -1 if the specified Value was found (i.e. adding will duplicate a value)
function FastLocateIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;

/// add an integer value in a sorted dynamic array of integers
// - returns the index where the Value was added successfully in Values[]
// - returns -1 if the specified Value was alredy present in Values[]
//  (we must avoid any duplicate for binary search)
// - if CoValues is set, its content will be moved to allow inserting a new
// value at CoValues[result] position
function AddSortedInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: integer; CoValues: PIntegerDynArray=nil): PtrInt;

/// insert an integer value at the specified index position of a dynamic array
// of integers
// - if Index is invalid, the Value is inserted at the end of the array
function InsertInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: Integer; Index: PtrInt; CoValues: PIntegerDynArray=nil): PtrInt;

/// add an integer value at the end of a dynamic array of integers
// - true if Value was added successfully in Values[], in this case
// length(Values) will be increased
function AddInteger(var Values: TIntegerDynArray; Value: integer;
  NoDuplicates: boolean=false): boolean; overload;

/// add an integer value at the end of a dynamic array of integers
// - this overloaded function will use a separate Count variable (faster)
// - true if Value was added successfully in Values[], in this case
// length(Values) will be increased
function AddInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: integer; NoDuplicates: boolean=false): boolean; overload;

/// delete any integer in Values[]
procedure DeleteInteger(var Values: TIntegerDynArray; Index: PtrInt); overload;

/// delete any integer in Values[]
procedure DeleteInteger(var Values: TIntegerDynArray; var ValuesCount: Integer; Index: PtrInt); overload;

/// fill some values with i,i+1,i+2...i+Count-1
procedure FillIncreasing(Values: PIntegerArray; StartValue, Count: integer);

/// copy some Int64 values into an unsigned integer array
procedure Int64ToUInt32(Values64: PInt64Array; Values32: PCardinalArray; Count: integer);

/// add the strings in the specified CSV text into a dynamic array of integer
procedure CSVToIntegerDynArray(CSV: PUTF8Char; var Result: TIntegerDynArray);

/// return the corresponding CSV text from a dynamic array of integer
// - you can set some custom Prefix and Suffix text
function IntegerDynArrayToCSV(const Values: TIntegerDynArray; ValuesCount: integer;
  const Prefix: RawUTF8=''; const Suffix: RawUTF8=''): RawUTF8;

type
  /// used to store and retrieve Words in a sorted array
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  TSortedWordArray = {$ifndef UNICODE}object{$else}record{$endif}
  public
    Values: TWordDynArray;
    Count: integer;
    /// add a value into the sorted array
    // - return the index of the new inserted value into the Values[] array
    // - return -(foundindex+1) if this value is already in the Values[] array
    function Add(aValue: Word): PtrInt;
    /// return the index if the supplied value in the Values[] array
    // - return -1 if not found
    function IndexOf(aValue: Word): PtrInt; {$ifdef HASINLINE}inline;{$endif}
  end;

/// convert a cardinal into a 32-bit variable-length integer buffer
function ToVarUInt32(Value: PtrUInt; Dest: PByte): PByte;

/// return the number of bytes necessary to store a 32-bit variable-length integer
// - i.e. the ToVarUInt32() buffer size
function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// return the number of bytes necessary to store some data with a its
// 32-bit variable-length integer legnth
function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert an integer into a 32-bit variable-length integer buffer
// - store negative values as cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;

/// convert a 32-bit variable-length integer buffer into a cardinal
function FromVarUInt32(var Source: PByte): PtrUInt;
  {$ifdef HASINLINE}inline;{$endif}

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
// ! result := Source^;
// ! inc(Source);
// ! if result>$7f then
// !   result := (result and $7F) or FromVarUInt32Up128(Source);
function FromVarUInt32Up128(var Source: PByte): PtrUInt;

/// convert a 32-bit variable-length integer buffer into a cardinal
// - this version must be called if Source^ has already been checked to be > $7f
function FromVarUInt32High(var Source: PByte): PtrUInt;

/// convert a 32-bit variable-length integer buffer into an integer
// - decode negative values from cardinal two-complement, i.e.
// 0=0,1=1,2=-1,3=2,4=-2...
function FromVarInt32(var Source: PByte): PtrInt; {$ifdef HASINLINE}inline;{$endif}

/// convert a UInt64 into a 64-bit variable-length integer buffer
function ToVarUInt64(Value: QWord; Dest: PByte): PByte;

/// convert a 64-bit variable-length integer buffer into a UInt64
function FromVarUInt64(var Source: PByte): QWord;

/// convert a Int64 into a 64-bit variable-length integer buffer
function ToVarInt64(Value: Int64; Dest: PByte): PByte; {$ifdef HASINLINE}inline;{$endif}

/// convert a 64-bit variable-length integer buffer into a Int64
function FromVarInt64(var Source: PByte): Int64; {$ifdef HASINLINE}inline;{$endif}

/// jump a value in the 32-bit or 64-bit variable-length integer buffer
function GotoNextVarInt(Source: PByte): pointer; {$ifdef HASINLINE}inline;{$endif}

/// jump a value in variable-length text buffer
function GotoNextVarString(Source: PByte): pointer; {$ifdef HASINLINE}inline;{$endif}

/// retrieve a variable-length text buffer
function FromVarString(var Source: PByte): RawUTF8;


{ ************ low-level RTTI types and conversion routines }

type
  /// function prototype to be used for TDynArray Sort and Find method
  // - common functions exist for base types: see e.g. SortDynArrayByte,
  // SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal, SortDynArrayInt64,
  // SortDynArrayDouble, SortDynArrayAnsiString, SortDynArrayAnsiStringI,
  // SortDynArrayUnicodeString, SortDynArrayUnicodeStringI,
  // SortDynArrayString, SortDynArrayStringI
  // - any custom type (even records) can be compared then sort by defining
  // such a custom function
  // - must return 0 if A=B, -1 if A<B, 1 if A>B
  TDynArraySortCompare = function(const A,B): integer;

  /// event oriented version of TDynArraySortCompare
  TEventDynArraySortCompare = function(const A,B): integer of object;

  /// internal enumeration used to specify some standard Delphi arrays
  // - will be used e.g. to match JSON serialization or TDynArray search
  // (see TDynArray and TDynArrayHash InitSpecific method)
  // - djByte .. djTimeLog match numerical JSON values
  // - djDateTime .. djSynUnicode match textual JSON values
  // - djCustom will be used for registered JSON serializer (invalid for
  // InitSpecific methods call)
  // - see also djPointer and djObject constant aliases for a pointer or
  // TObject field hashing / comparison
  TDynArrayKind = (
    djNone,
    djByte, djWord, djInteger, djCardinal, djInt64, djDouble, djCurrency,
    djTimeLog, djDateTime, djRawUTF8, djWinAnsi, djString,
    djWideString, djSynUnicode,
    djCustom);

const
  /// TDynArrayKind alias for a pointer field hashing / comparison
  djPointer = {$ifdef CPU64}djInt64{$else}djCardinal{$endif};

  /// TDynArrayKind alias for a TObject field hashing / comparison
  djObject = djPointer;

type
  /// a wrapper around a dynamic array with one dimension
  // - provide TList-like methods using fast RTTI information
  // - can be used to fast save/retrieve all memory content to a TStream
  // - note that the "const Elem" is not checked at compile time nor runtime:
  // you must ensure that Elem matchs the element type of the dynamic array
  // - can use external Count storage to make Add() and Delete() much faster
  // (avoid most reallocation of the memory buffer)
  // - Note that TDynArray is just a wrapper around an existing dynamic array:
  // methods can modify the content of the associated variable but the TDynArray
  // doesn't contain any data by itself. It is therefore aimed to initialize
  // a TDynArray wrapper on need, to access any existing dynamic array.
  TDynArray = object
  protected
    Value: PPointer;
    TypeInfo: pointer;
    ElemSize: PtrUInt;
    ElemType: pointer;
    fCompare: TDynArraySortCompare;
    fCountP: PInteger;
    fSorted: boolean;
    fKnownType: TDynArrayKind;
    function GetCount: integer; {$ifdef HASINLINE}inline;{$endif}
    procedure SetCount(aCount: integer);
    function GetCapacity: integer;
    procedure SetCapacity(aCapacity: integer);
    procedure SetCompare(const aCompare: TDynArraySortCompare); {$ifdef HASINLINE}inline;{$endif}
    function FindIndex(const Elem; aIndex: PIntegerDynArray;
      aCompare: TDynArraySortCompare): integer;
    function ToKnownType: TDynArrayKind;
    /// faster equivalency of System.DynArraySetLength() function
    procedure InternalSetLength(NewLength: PtrUInt);
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - the dynamic array must have been defined with its own type
    // (e.g. TIntegerDynArray = array of Integer)
    // - if aCountPointer is set, it will be used instead of length() to store
    // the dynamic array items count - it will be much faster when adding
    // elements to the array, because the dynamic array won't need to be
    // resized each time - but in this case, you should use the Count property
    // instead of length(array) or high(array) when accessing the data: in fact
    // length(array) will store the memory size reserved, not the items count
    // - if aCountPointer is set, its content will be set to 0, whatever the
    // array length is, or the current aCountPointer^ value is
    // - a sample usage may be:
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A);
    // ! (...)
    // - a sample usage may be (using a count variable):
    // !var DA: TDynArray;
    // !    A: TIntegerDynArray;
    // !    ACount: integer;
    // !    i: integer;
    // !begin
    // !  DA.Init(TypeInfo(TIntegerDynArray),A,@ACount);
    // !  for i := 1 to 100000 do
    // !    DA.Add(i); // MUCH faster using the ACount variable
    // ! (...)   // now you should use DA.Count or Count instead of length(A)
    procedure Init(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts to specify how comparison should occur, using
    // TDynArrayKind  kind of first field
    // - djNone and djCustom are too vague, and would raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that the aKind parameter matches the dynamic array element definition
    // - aCaseInsensitive will be used for djRawUTF8..djSynUnicode comparison
    procedure InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind;
      aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
    /// initialize the wrapper to point to no dynamic array
    procedure Void;
    /// check if the wrapper points to a dynamic array
    function IsVoid: boolean;
    /// add an element to the dynamic array
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Add(i+10) e.g.)
    // - returns the index of the added element in the dynamic array
    // - note that because of dynamic array internal memory managment, adding
    // will be a bit slower than e.g. with a TList: the list is reallocated
    // every time a record is added - but in practice, with FastMM4 or
    // SynScaleMM, there is no big speed penalty - for even better speed, you
    // can also specify an external count variable in Init(...,@Count) method
    function Add(const Elem): integer;
    /// add an element to the dynamic array
    // - this version add a void element to the array, and returns its index
    function New: integer;
    /// add elements from a given dynamic array
    // - the supplied source DynArray MUST be of the same exact type as the
    // current used for this TDynArray
    // - you can specify the start index and the number of items to take from
    // the source dynamic array (leave as -1 to add till the end)
    procedure AddArray(const DynArray; aStartIndex: integer=0; aCount: integer=-1);
    /// add an element to the dynamic array at the position specified by Index
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Insert(10,i+10) e.g.)
    procedure Insert(Index: Integer; const Elem);
    /// delete the whole dynamic array content
    procedure Clear;
    /// delete one item inside the dynamic array
    // - the deleted element is finalized if necessary
    procedure Delete(Index: Integer);
    /// search for an element value inside the dynamic array
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - will search for all properties content of the eLement: TList.IndexOf()
    // searches by address, this method searches by content using the RTTI
    // element description (and not the Compare property function)
    // - use the Find() method if you want the search via the Compare property
    // function, or e.g. to search only with some part of the element content
    // - will work with simple types: binaries (byte, word, integer, Int64,
    // Currency, array[0..255] of byte, packed records with no reference-counted
    // type within...), string types (e.g. array of string), and packed records
    // with binary and string types within (like TFileVersion)
    // - won't work with not packed types (like a shorstring, or a record
    // with byte or word fields with {$A+}): in this case, the padding data
    // (i.e. the bytes between the aligned feeds can be filled as random, and
    // there is no way with standard RTTI do know which they are)
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write IndexOf(i+10) e.g.)
    function IndexOf(const Elem): integer;
    /// search for an element value inside the dynamic array
    // - this method will use the Compare property function for the search
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - if the array is sorted, it will use fast binary search
    // - if the array is not sorted, it will use slower iterating search
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function Find(const Elem): integer; overload;
    /// search for an element value inside the dynamic array, from an external
    // indexed lookup table
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - this method will use a custom comparison function, with an external
    // integer table, as created by the CreateOrderedIndex() method: it allows
    // multiple search orders in the same dynamic array content
    // - if an indexed lookup is supplied, it must already be sorted:
    // this function will then use fast binary search
    // - if an indexed lookup is not supplied (i.e aIndex=nil),
    // this function will use slower but accurate iterating search
    // - warning; the lookup index should be synchronized if array content
    // is modified (in case of adding or deletion)
    function Find(const Elem; const aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare): integer; overload;
    /// search for an element value, then fill all properties if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Elem content matches, all Elem fields will be filled with the record 
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - if the array is sorted, it will use fast binary search
    // - if the array is not sorted, it will use slower iterating search
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndFill(var Elem; aIndex: PIntegerDynArray=nil;
      aCompare: TDynArraySortCompare=nil): integer;
    /// search for an element value, then delete it if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Elem content matches, this item will be deleted from the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index deleted (0..Count-1), or -1 if Elem was not found
    // - if the array is sorted, it will use fast binary search
    // - if the array is not sorted, it will use slower iterating search
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndDelete(var Elem; aIndex: PIntegerDynArray=nil;
      aCompare: TDynArraySortCompare=nil): integer;
    /// search for an element value, then update it if match
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if Elem content matches, this item will be updated with the supplied
    // value
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - if the array is sorted, it will use fast binary search
    // - if the array is not sorted, it will use slower iterating search
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndUpdate(const Elem; aIndex: PIntegerDynArray=nil;
      aCompare: TDynArraySortCompare=nil): integer;
    /// search for an element value, then add it if none matched
    // - this method will use the Compare property function for the search,
    // or the supplied indexed lookup table and its associated compare function
    // - if no Elem content matches, the item will added to the array
    // - can be used e.g. as a simple dictionary: if Compare will match e.g. the
    // first string field (i.e. set to SortDynArrayString), you can fill the
    // first string field with the searched value (if returned index is >= 0)
    // - return the index found (0..Count-1), or -1 if Elem was not found and
    // the supplied element has been succesfully added
    // - if the array is sorted, it will use fast binary search
    // - if the array is not sorted, it will use slower iterating search
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindAndAddIfNotExisting(const Elem; aIndex: PIntegerDynArray=nil;
      aCompare: TDynArraySortCompare=nil): integer;
    /// sort the dynamic array elements, using the Compare property function
    // - it will change the dynamic array content, and exchange all elements
    // in order to be sorted in increasing order according to Compare function
    procedure Sort;
    /// will reverse all array elements, in place
    procedure Reverse;
    /// sort the dynamic array elements using a lookup array of indexes
    // - it won't change the dynamic array content: only create or update
    // the given integer lookup array, using the specified comparison function
    // - you should provide either a void either a valid lookup table, that is
    // a table with one to one lookup (e.g. created with FillIncreasing)
    // - if the lookup table has less elements than the main dynamic array,
    // its content will be recreated
    procedure CreateOrderedIndex(var aIndex: TIntegerDynArray; aCompare: TDynArraySortCompare);
    /// save the dynamic array content into a (memory) stream
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length
    // - Stream position will be set just after the added data
    // - is optimized for memory streams, but will work with any kind of TStream
    procedure SaveToStream(Stream: TStream);
    /// load the dynamic array content from a (memory) stream
    // - stream content must have been created using SaveToStream method
    // - will handle array of binaries values (byte, word, integer...), array of
    // strings or array of packed records, with binaries and string properties
    // - will use a proprietary binary format, with some variable-length encoding
    // of the string length
    procedure LoadFromStream(Stream: TCustomMemoryStream);
    /// save the dynamic array content into an allocated memory buffer
    // - Dest buffer must have been allocated to contain at least the number
    // of bytes returned by the SaveToLength method
    // - return a pointer at the end of the data written in Dest, nil in case
    // of an invalid input buffer
    function SaveTo(Dest: PAnsiChar): PAnsiChar; overload;
    /// compute the number of bytes needed to save a dynamic array content
    function SaveToLength: integer;
    /// save the dynamic array content into a RawByteString
    function SaveTo: RawByteString; overload;
    /// load the dynamic array content from a memory buffer
    // - return nil if the Source buffer is incorrect (invalid type or internal
    // checksum e.g.)
    // - in case of success, return the memory buffer pointer just after the
    // read content
    // - return a pointer at the end of the data read from Source, nil on error
    function LoadFrom(Source: PAnsiChar): PAnsiChar;
    /// load the dynamic array content from an UTF-8 encoded JSON buffer
    // - expect the format as saved by TTextWriter.AddDynArrayJSON method, i.e.
    // handling TIntegerDynArray, TInt64DynArray, TCardinalDynArray,
    // TDoubleDynArray, TCurrencyDynArray, TWordDynArray, TByteDynArray,
    // TRawUTF8DynArray, TWinAnsiDynArray, TRawByteStringDynArray,
    // TStringDynArray, TWideStringDynArray, TSynUnicodeDynArray,
    // TTimeLogDynArray and TDateTimeDynArray as JSON array - or any customized
    // valid JSON serialization as set by TTextWriter.RegisterCustomJSONSerializer
    // - or any other kind of array as Base64 encoded binary stream precessed
    // via JSON_BASE64_MAGIC (UTF-8 encoded \uFFF0 special code)
    // - typical handled content could be
    // ! '[1,2,3,4]' or '["\uFFF0base64encodedbinary"]'
    // - return a pointer at the end of the data read from P, nil in case
    // of an invalid input buffer
    // - warning: the content of P^ will be modified during parsing: please
    // make a local copy if it will be needed later
    function LoadFromJSON(P: PUTF8Char): PUTF8Char;
    /// set all content of one dynamic array to the current array
    // - both must be of the same exact type
    procedure Copy(Source: TDynArray);
    ///  select a sub-section (slice) of a dynamic array content
    procedure Slice(var Dest; aCount: Cardinal; aFirstIndex: cardinal=0);
    {$ifndef DELPHI5OROLDER}
    /// compare the content of the two arrays, returning TRUE if both match
    // - this method compares first using any supplied Compare property,
    // then by content using the RTTI element description of the whole record
    function Equals(const B: TDynArray): boolean;
    {$endif}
    /// compare the content of two elements, returning TRUE if both values equal
    // - this method compares first using any supplied Compare property,
    // then by content using the RTTI element description of the whole record
    function ElemEquals(const A,B): boolean;
    /// will reset the element content
    procedure ElemClear(var Elem);
    /// will copy one element content
    procedure ElemCopy(const A; var B);
    /// save an array element into a serialized buffer
    // - you can use ElemLoad method later to retrieve its content
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function ElemSave(const Elem): RawByteString;
    /// load an array element as saved by the ElemSave method
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    procedure ElemLoad(Source: PAnsiChar; var Elem); overload;
    /// load an array element as saved by the ElemSave method
    // - this overloaded method will retrieve the element as a memory buffer
    // and caller MUST call ElemLoadClear() method to finalize its content
    function ElemLoad(Source: PAnsiChar): RawByteString; overload;
    /// release memory allocated by ElemLoad(): RawByteString
    procedure ElemLoadClear(var ElemLoaded: RawByteString);
    /// search for an array element as saved by the ElemSave method
    // - same as ElemLoad() + Find()/IndexOf() + ElemLoadClear()
    // - will call Find() method if Compare property is set
    // - will call generic IndexOf() method if no Compare property is set
    function ElemLoadFind(Source: PAnsiChar): integer;

    /// retrieve or set the number of elements of the dynamic array
    // - same as length(DynArray) or SetLenght(DynArray)
    property Count: integer read GetCount write SetCount;
    /// the internal buffer capacity
    // - if no external Count pointer was set with Init, is the same as Count
    // - if an external Count pointer is set, you can set a value to this
    // property before a massive use of the Add() method e.g.
    // - if no external Count pointer is set, set a value to this property
    // will affect the Count value, i.e. Add() will append after this count
    property Capacity: integer read GetCapacity write SetCapacity;
    /// the compare function to be used for Sort and Find methods
    // - by default, no comparison function is set
    // - common functions exist for base types: e.g. SortDynArrayByte,
    // SortDynArrayWord, SortDynArrayInteger, SortDynArrayCardinal,
    // SortDynArrayInt64, SortDynArrayDouble, SortDynArrayAnsiString,
    // SortDynArrayAnsiStringI, SortDynArrayString, SortDynArrayStringI,
    // SortDynArrayUnicodeString, SortDynArrayUnicodeStringI
    property Compare: TDynArraySortCompare read fCompare write SetCompare;
    /// must be TRUE if the array is currently in sorted order according to
    // the compare function
    // - Add/Delete/Insert/Load* methods will reset this property to false
    // - Sort method will set this property to true
    // - you MUST set this property to false if you modify the dynamic array
    // content in your code, so that Find() won't try to use binary search in
    // an usorted array, and miss its purpose
    property Sorted: boolean read fSorted write fSorted;
  end;


  /// function prototype to be used for hashing of an element
  // - it must return a cardinal hash, with as less collision as possible
  // - a good candidate is our crc32() function in optimized asm in SynZip unit
  // - TDynArrayHashed.Init will use kr32() if no custom function is supplied,
  // which is the standard Kernighan & Ritchie hash function
  THasher = function(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;

  /// function prototype to be used for hashing of a dynamic array element
  // - this function must use the supplied hasher on the Elem data
  TDynArrayHashOne = function(const Elem; Hasher: THasher): cardinal;

  /// event handler to be used for hashing of a dynamic array element
  TOnDynArrayHashOne = function(const Elem): cardinal of object;

  /// internal structure used to store one item hash
  // - used e.g. by TDynArrayHashed or TObjectHash via TSynHashDynArray
  TSynHash = record
    /// unsigned integer hash of the item
    Hash: cardinal;
    /// index of the item in the main storage array
    Index: cardinal;
  end;

  /// internal structure used to store hashs of items
  // - used e.g. by TDynArrayHashed or TObjectHash
  TSynHashDynArray = array of TSynHash;

  /// used to access any dynamic arrray elements using fast hash
  // - by default, binary sort could be used for searching items for TDynArray:
  // using a hash is faster on huge arrays for implementing a dictionary
  // - in this current implementation, modification (update or delete) of an
  // element is not handled yet: you should rehash all content - only
  // TDynArrayHashed.FindHashedForAdding / FindHashedAndUpdate /
  // FindHashedAndDelete will refresh the internal hash
  // - this object extends the TDynArray type, since presence of Hashs[] dynamic
  // array will increase code size if using TDynArrayHashed instead of TDynArray
  // - in order to have the better performance, you should use an external Count
  // variable, AND set the Capacity property to the expected maximum count (this
  // will avoid most ReHash calls for FindHashedForAdding+FindHashedAndUpdate)
  TDynArrayHashed = object(TDynArray)
  protected
    fHashElement: TDynArrayHashOne;
    fHasher: THasher;
    fHashs: TSynHashDynArray;
    fEventCompare: TEventDynArraySortCompare;
    function HashOne(const Elem): cardinal;
    function HashFind(aHashCode: cardinal; const Elem): integer;
    procedure HashAdd(const Elem; aHashCode: Cardinal; var result: integer);
    function GetHashFromIndex(aIndex: Integer): Cardinal;
    procedure HashInit;
  public
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts some hash-dedicated parameters: aHashElement to
    // set how to hash each element, aCompare to handle hash collision
    // - if no aHashElement is supplied, it will hash according to the RTTI, i.e.
    // strings or binary types, and the first field for records (strings included)
    // - if no aCompare is supplied, it will use default Equals() method
    // - if no THasher function is supplied, it will use the one supplied in
    // DefaultHasher global variable, set to kr32() by default - i.e. the well
    // known Kernighan & Ritchie hash function
    // - if CaseInsensitive is set to TRUE, it will ignore difference in 7 bit
    // alphabetic characters (e.g. compare 'a' and 'A' as equal)
    procedure Init(aTypeInfo: pointer; var aValue;
      aHashElement: TDynArrayHashOne=nil; aCompare: TDynArraySortCompare=nil;
      aHasher: THasher=nil; aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
    /// initialize the wrapper with a one-dimension dynamic array
    // - this version accepts to specify how both hashing and comparison should
    // occur, using TDynArrayKind  kind of first field
    // - djNone and djCustom are too vague, and would raise an exception
    // - no RTTI check is made over the corresponding array layout: you shall
    // ensure that the aKind parameter matches the dynamic array element definition
    // - aCaseInsensitive will be used for djRawUTF8..djSynUnicode comparison
    procedure InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind;
      aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
    /// will compute all hash from the current elements of the dynamic array
    // - is called within the TDynArrayHashed.Init method to initialize the
    // internal hash array
    // - can be called on purpose, when modifications have been performed on
    // the dynamic array content (e.g. in case of element deletion or update,
    // or after calling LoadFrom/Clear method) - this is not necessary after
    // FindHashedForAdding / FindHashedAndUpdate / FindHashedAndDelete methods
    procedure ReHash(aHasher: TOnDynArrayHashOne=nil);
    /// search for an element value inside the dynamic array using hashing
    // - ELem should be of the same exact type than the dynamic array, or at
    // least matchs the fields used by both the hash function and Equals method:
    // e.g. if the searched/hashed field in a record is a string as first field,
    // you may use a string variable as Elem: other fields will be ignored
    // - returns -1 if not found, or the index in the dynamic array if found
    function FindHashed(const Elem): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // fill Elem with the found content
    // - return the index found (0..Count-1), or -1 if Elem was not found
    // - warning: Elem must be of the same exact type than the dynamic array,
    // and must be a reference to a variable (you can't write Find(i+10) e.g.)
    function FindHashedAndFill(var Elem): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // add a void entry to the array if was not found
    // - this method will use hashing for fast retrieval
    // - ELem should be of the same exact type than the dynamic array, or at
    // least matchs the fields used by both the hash function and Equals method:
    // e.g. if the searched/hashed field in a record is a string as first field,
    // you may use a string variable as Elem: other fields will be ignored
    // - returns either the index in the dynamic array if found (and set wasAdded
    // to false), either the newly created index in the dynamic array (and set
    // wasAdded to true)
    // - for faster process (avoid ReHash), please set the Capacity property
    // - warning: in contrast to the Add() method, if an entry is added to the
    // array (wasAdded=true), the entry is left VOID: you must set the field
    // content to expecting value - in short, Elem is used only for searching,
    // not for filling the newly created entry in the array
    function FindHashedForAdding(const Elem; out wasAdded: boolean;
      aHashCode: cardinal=0): integer;
    /// search for a given element name, make it unique, and add it to the array
    // - expected element layout is to have a RawUTF8 field at first position
    // - the aName is searched (using hashing) to be unique, and if not the case,
    // some suffix is added to make it unique
    // - use internaly FindHashedForAdding method
    // - this version will set the field content with the unique value
    // - returns a pointer to the newly added element
    function AddAndMakeUniqueName(aName: RawUTF8): pointer;
    /// search for an element value inside the dynamic array using hashing, then
    // update any matching item, or add the item if none matched
    // - if AddIfNotExisting is FALSE, returns the index found (0..Count-1),
    // or -1 if Elem was not found - update will force slow rehash all content
    // - if AddIfNotExisting is TRUE, returns the index found (0..Count-1),
    // or the index newly created/added is the Elem value was not matching -
    // add won't rehash all content - for even faster process (avoid ReHash),
    // please set the Capacity property
    // - warning: Elem must be of the same exact type than the dynamic array, and
    // must refer to a variable (you can't write FindHashedAndUpdate(i+10) e.g.)
    function FindHashedAndUpdate(var Elem; AddIfNotExisting: boolean): integer;
    /// search for an element value inside the dynamic array using hashing, and
    // delete it if matchs
    // - return the index deleted (0..Count-1), or -1 if Elem was not found
    // - this will rehash all content: this method could be slow in the current
    // implementation
    // - warning: Elem must be of the same exact type than the dynamic array, and
    // must refer to a variable (you can't write FindHashedAndDelete(i+10) e.g.)
    function FindHashedAndDelete(var Elem): integer;
    /// retrieve the hash value of a given item, from its index
    property Hash[aIndex: Integer]: Cardinal read GetHashFromIndex;
    /// alternative event-oriented Compare function to be used for Sort and Find
    // - will be used instead of Compare, to allow object-oriented callbacks
    property EventCompare: TEventDynArraySortCompare read fEventCompare write fEventCompare;
  end;

  /// abstract class able to use hashing to find an object in O(1) speed
  // - all protected abstract methods shall be overriden and implemented
  TObjectHash = class
  protected
    fHashs: TSynHashDynArray;
    procedure HashInit(aCountToHash: integer);
    function HashFind(aHashCode: cardinal; Item: TObject): integer;
    /// abstract method to hash an item
    // - note that the overriden method shall not return 0 (mark void fHashs[])
    function Hash(Item: TObject): cardinal; virtual; abstract;
    /// abstract method to compare two items
    function Compare(Item1,Item2: TObject): boolean; virtual; abstract;
    /// abstract method to get an item
    // - shall return nil if Index is out of range (e.g. >= Count)
    // - will be called e.g. by Find() with Compare() to avoid collision
    function Get(Index: integer): TObject; virtual; abstract;
    /// used to retrieve the number of items
    function Count: integer; virtual; abstract;
  public
    /// search one item in the internal hash array
    function Find(Item: TObject): integer;
    /// to be called when an item is modified
    // - for Delete/Update will force a full rehash on next Find() call
    procedure Invalidate;
    /// to be called when an item is added
    // - return FALSE if this item is already existing (i.e. insert error)
    // - return TRUE if has been added to the internal hash table
    // - the index of the latest added item should be Count-1
    function JustAdded: boolean;
  end;

  /// store one Name/Value pair, as used by TSynNameValue class
  TSynNameValueItem = record
    /// the name of the Name/Value pair
    // - this property is hashed by TSynNameValue for fast retrieval
    Name: RawUTF8;
    /// the value of the Name/Value pair
    Value: RawUTF8;
    /// any associated Pointer or numerical value
    Tag: PtrInt;
  end;

  /// Name/Value pairs storage, as used by TSynNameValue class
  TSynNameValueItemDynArray = array of TSynNameValueItem;

  /// pseudo-class used to store Name/Value RawUTF8 pairs
  // - use internaly a TDynArrayHashed instance for fast retrieval
  // - is therefore faster than TRawUTF8List
  // - is defined as an object, not as a class: you can use this in any
  // class, without the need to destroy the content
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  TSynNameValue = {$ifndef UNICODE}object{$else}record{$endif}
    fDynArray: TDynArrayHashed;
    function GetBlobData: RawByteString;
    procedure SetBlobData(const aValue: RawByteString);
  public
    /// the internal Name/Value storage
    List: TSynNameValueItemDynArray;
    /// the number of Name/Value pairs
    Count: integer;
    /// initialize the storage
    procedure Init(aCaseSensitive: boolean);
    /// add an element to the array
    // - if aName already exists, its associated Value will be updated
    procedure Add(const aName, aValue: RawUTF8; aTag: PtrInt=0);
    /// search for a Name, return the index in List
    function Find(const aName: RawUTF8): integer;
    /// search for a Name, return the associated Value
    function Value(const aName: RawUTF8): RawUTF8;
    /// returns true if the Init() method has been called
    function Initialized: boolean;
    /// can be used to set or retrieve all stored data as one BLOB content
    property BlobData: RawByteString read GetBlobData write SetBlobData;
  end;



{$ifndef PUREPASCAl}
/// helper to retrieve the text of an enumerate item
// - you'd better use RTTI related classes of SQLite3Commons unit
function GetEnumName(aTypeInfo: pointer; aIndex: integer): PShortString;
{$endif}


/// check equality of two records by content
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties
// - will use binary-level comparison: it could fail to match two floating-point
// values because of rounding issues (Currency won't have this problem)
function RecordEquals(const RecA, RecB; TypeInfo: pointer): boolean;

/// save a record content into a RawByteString
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties (but not with internal raw pointers, of course)
// - will use a proprietary binary format, with some variable-length encoding
// of the string length
// - warning: will encode generic string fields as AnsiString (one byte per char)
// prior to Delphi 2009, and as UnicodeString (two bytes per char) since Delphi
// 2009: if you want to use this function between UNICODE and NOT UNICODE
// versions of Delphi, you should use some explicit types like RawUTF8,
// WinAnsiString or even RawUnicode
function RecordSave(const Rec; TypeInfo: pointer): RawByteString; overload;

/// save a record content into a destination memory buffer
// - Dest must be at least RecordSaveLength() bytes long
// - will handle packed records, with binaries (byte, word, integer...) and
// string types properties (but not with internal raw pointers, of course)
// - will use a proprietary binary format, with some variable-length encoding
// of the string length
// - warning: will encode generic string fields as AnsiString (one byte per char)
// prior to Delphi 2009, and as UnicodeString (two bytes per char) since Delphi
// 2009: if you want to use this function between UNICODE and NOT UNICODE
// versions of Delphi, you should use some explicit types like RawUTF8,
// WinAnsiString or even RawUnicode
function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer): PAnsiChar; overload;

/// compute the number of bytes needed to save a record content
// using the RecordSave() function
// - will return 0 in case of an invalid (not handled) record type (e.g. if
// it contains a variant or a dynamic array)
function RecordSaveLength(const Rec; TypeInfo: pointer): integer;

/// fill a record content from a memory buffer as saved by RecordSave()
// - return nil if the Source buffer is incorrect
// - in case of success, return the memory buffer pointer just after the
// read content
function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: pointer): PAnsiChar;

/// fill a record content from a JSON serialization as saved by TTextWriter.AddRecordJSON
// - will handle both default (Bin64 encoding of Record Save binary) and
// custom true JSON format (as set by TTextWriter.RegisterCustomJSONSerializer)
function RecordLoadJSON(var Rec; JSON: PUTF8Char; TypeInfo: pointer;
  EndOfObject: PUTF8Char=nil): PUTF8Char;

/// copy a record content from source to Dest
// - this unit includes a fast optimized asm version
procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);

/// clear a record content
procedure RecordClear(var Dest; TypeInfo: pointer);


/// compare two "array of byte" elements
function SortDynArrayByte(const A,B): integer;

/// compare two "array of word" elements
function SortDynArrayWord(const A,B): integer;

/// compare two "array of integer" elements
function SortDynArrayInteger(const A,B): integer;

/// compare two "array of cardinal" elements
function SortDynArrayCardinal(const A,B): integer;

/// compare two "array of Int64 or array of Currency" elements
function SortDynArrayInt64(const A,B): integer;

/// compare two "array of TObject/pointer" elements
function SortDynArrayPointer(const A,B): integer;

/// compare two "array of double" elements
function SortDynArrayDouble(const A,B): integer;

/// compare two "array of AnsiString" elements, with case sensitivity
function SortDynArrayAnsiString(const A,B): integer;

/// compare two "array of AnsiString" elements, with no case sensitivity
function SortDynArrayAnsiStringI(const A,B): integer;

/// compare two "array of WideString/UnicodeString" elements, with case sensitivity
function SortDynArrayUnicodeString(const A,B): integer;

/// compare two "array of WideString/UnicodeString" elements, with no case sensitivity
function SortDynArrayUnicodeStringI(const A,B): integer;

/// compare two "array of generic string" elements, with case sensitivity
// - the expected string type is the generic VCL string
function SortDynArrayString(const A,B): integer;

/// compare two "array of generic string" elements, with no case sensitivity
// - the expected string type is the generic VCL string
function SortDynArrayStringI(const A,B): integer;

/// initialize the structure with a one-dimension dynamic array
// - the dynamic array must have been defined with its own type
// (e.g. TIntegerDynArray = array of Integer)
// - if aCountPointer is set, it will be used instead of length() to store
// the dynamic array items count - it will be much faster when adding
// elements to the array, because the dynamic array won't need to be
// resized each time - but in this case, you should use the Count property
// instead of length(array) or high(array) when accessing the data: in fact
// length(array) will store the memory size reserved, not the items count
// - if aCountPointer is set, its content will be set to 0, whatever the
// array length is, or the current aCountPointer^ value is
// - a typical usage could be:
// !var A: TIntegerDynArray;
// !begin
// !  with DynArray(TypeInfo(TIntegerDynArray),A) do
// !  begin
// !    (...)
// !  end;
function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray;
  {$ifdef HASINLINE}inline;{$endif}

/// wrap a simple dynamic array BLOB content as stored by TDynArray.SaveTo
// - a "simple" dynamic array contains data with no reference count, e.g. byte,
// word, integer, cardinal, Int64, double or Currency
// - same as TDynArray.LoadFrom() with no memory allocation nor memory copy: so
// is much faster than creating a temporary dynamic array to load the data
// - will return nil if no or invalid data, or a pointer to the data
// array otherwise, with the items number stored in Count and the individual
// element size in ElemSize (e.g. 2 for a TWordDynArray)
function SimpleDynArrayLoadFrom(Source: PAnsiChar; aTypeInfo: pointer;
  var Count, ElemSize: integer): pointer;

/// wrap an Integer dynamic array BLOB content as stored by TDynArray.SaveTo
// - same as TDynArray.LoadFrom() with no memory allocation nor memory copy: so
// is much faster than creating a temporary dynamic array to load the data
// - will return nil if no or invalid data, or a pointer to the integer
// array otherwise, with the items number stored in Count
// - a bit faster than SimpleDynArrayLoadFrom(Source,TypeInfo(TIntegerDynArray),Count)
function IntegerDynArrayLoadFrom(Source: PAnsiChar; var Count: integer): PIntegerArray;

/// search in a RawUTF8 dynamic array BLOB content as stored by TDynArray.SaveTo
// - same as search within TDynArray.LoadFrom() with no memory allocation nor
// memory copy: so is much faster
// - will return -1 if no match or invalid data, or the matched entry index
function RawUTF8DynArrayLoadFromContains(Source: PAnsiChar;
  Value: PUTF8Char; ValueLen: integer; CaseSensitive: boolean): integer;


{ ****************** text buffer and JSON functions and classes ********* }

const
  /// maximum number of fields in a database Table
  // - is included in SynCommons so that all DB-related work will be able to
  // share the same low-level types and functions (e.g. TSQLFieldBits,
  // TJSONWriter, TSynTableStatement, TSynTable)
  // - default is 64, but can be set to any value (64, 128, 192 and 256 optimized)
  // - this constant is used internaly to optimize memory usage in the
  // generated asm code, and statically allocate some arrays for better speed
  MAX_SQLFIELDS = 64;

  /// UTF-8 encoded \uFFF0 special code -> mark Base64 TDynArray.SaveTo in JSON
  // - Unicode special char U+FFF0 is UTF-8 encoded as EF BF B0 bytes 
  JSON_BASE64_MAGIC = $b0bfef;

  /// '"' + UTF-8 encoded \uFFF0 special code
  JSON_BASE64_MAGIC_QUOTE = ord('"')+cardinal(JSON_BASE64_MAGIC) shl 8;

  /// '"' + UTF-8 encoded \uFFF0 special code
  JSON_BASE64_MAGIC_QUOTE_VAR: cardinal = JSON_BASE64_MAGIC_QUOTE;


type
  /// used to store bit set for all available fiels in a Table
  // - with current MAX_SQLFIELDS value, 256 bits uses 64 bytes of memory
  TSQLFieldBits = set of 0..MAX_SQLFIELDS-1;

  PSQLFieldBits = ^TSQLFieldBits;

  TTextWriter = class;

  /// method prototype for custom serialization of a dynamic array item
  // - each element of the dynamic array will be called as aValue parameter
  // of this callback
  // - can be used also at record level, if the record has a type information
  // (i.e. shall contain a managed type within its fields)
  // - to be used with TTextWriter.RegisterCustomJSONSerializer() method
  // - note that the generated JSON content will be appended after a '[' and
  // before a ']' as a normal JSON arrray, but each item can be any JSON
  // structure (i.e. a number, a string, but also an object or an array)
  // - implementation code could call aWriter.Add/AddJSONEscapeString...
  // - implementation code shall follow the same exact format for the
  // associated TDynArrayJSONCustomReader callback
  TDynArrayJSONCustomWriter = procedure(const aWriter: TTextWriter; const aValue) of object;

  /// method prototype for custom unserialization of a dynamic array item
  // - each element of the dynamic array will be called as aValue parameter
  // of this callback
  // - can be used also at record level, if the record has a type information
  // (i.e. shall contain a managed type within its fields)
  // - to be used with TTextWriter.RegisterCustomJSONSerializer() method
  // - implementation code could call e.g. GetJSONField() low-level function, and
  // returns a pointer to the last handled element of the JSON input buffer,
  // as such (aka EndOfBuffer variable as expected by GetJSONField):
  // ! var V: TFV absolute aValue;
  // ! begin
  // !   (...)
  // !   V.Detailed := UTF8ToString(GetJSONField(P,P));
  // !   if P=nil then
  // !     exit;
  // !   aValid := true;
  // !   result := P; // ',' or ']' for last item of array
  // ! end;
  // - implementation code shall follow the same exact format for the
  // associated TDynArrayJSONCustomWriter callback
  TDynArrayJSONCustomReader = function(P: PUTF8Char; var aValue;
    out aValid: Boolean): PUTF8Char of object;

  /// kind of adding in a TTextWriter
  TTextWriterKind = (twNone, twJSONEscape, twOnSameLine);

  /// simple writer to a Stream, specialized for the TEXT format
  // - use an internal buffer, faster than string+string
  // - some dedicated methods is able to encode any data with JSON escape
  TTextWriter = class
  private
    procedure SetStream(const Value: TStream);
  protected
    B, BEnd: PUTF8Char;
    fStream: TStream;
    fInitialStreamPosition: integer;
    fStreamIsOwned: boolean;
    // internal temporary buffer
    fTempBufSize: Integer;
    fTempBuf: PUTF8Char;
    // [0..4] for 'u0001' four-hex-digits template, [5..7] for one UTF-8 char
    BufUnicode: array[0..7] of AnsiChar;
    /// flush and go to next char
    function FlushInc: PUTF8Char;
    function GetLength: integer;
  public
    /// the data will be written to the specified Stream
    // - aStream may be nil: in this case, it MUST be set before using any
    // Add*() method
    // - default internal buffer size if 8192
    constructor Create(aStream: TStream; aBufSize: integer=8192);
    /// the data will be written to an internal TRawByteStringStream
    // - TRawByteStringStream.DataString method will be used by TTextWriter.Text
    // to retrieve directly the content without any data move nor allocation
    constructor CreateOwnedStream;
    /// release fStream is is owned
    destructor Destroy; override;
    /// retrieve the data as a string
    // - only works if the associated Stream Inherits from TMemoryStream or
    // TRawByteStringStream: will return '' if it is not the case
    function Text: RawUTF8;
    /// write pending data to the Stream
    // - will append the internal memory buffer to the Stream
    // - if you don't call Flush, some pending characters may be not yet
    // copied to the Stream: you should call it before using the Stream property
    procedure Flush;
    /// append one char to the buffer
    procedure Add(c: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append two chars to the buffer
    procedure Add(c1,c2: AnsiChar); overload;
      {$ifdef HASINLINE}inline;{$endif}
    /// append an Integer Value as a String
    procedure Add(Value: Int64); overload;
    /// append an Integer Value as a String
    procedure Add(Value: integer); overload;
    /// append a Currency from its Int64 in-memory representation
    procedure AddCurr64(Value: PInt64); overload;
    /// append a Currency from its Int64 in-memory representation
    procedure AddCurr64(const Value: Int64); overload;
    /// append a Currency from its Int64 in-memory representation
    procedure AddCurr64(const Value: currency); overload;
    /// append a TTimeLog value, expanded as Iso-8601 encoded text
    procedure AddTimeLog(Value: PInt64);
    /// append a TDateTime value, expanded as Iso-8601 encoded text
    procedure AddDateTime(Value: PDateTime; FirstChar: AnsiChar='T'; QuoteChar: AnsiChar=#0); overload;
    /// append a TDateTime value, expanded as Iso-8601 encoded text
    procedure AddDateTime(const Value: TDateTime); overload;
    /// append an Unsigned Integer Value as a String
    procedure AddU(Value: cardinal);
    /// append a floating-point Value as a String
    procedure Add(Value: double); overload;
    /// append a floating-point text buffer
    // - will correct on the fly '.5' -> '0.5' and '-.5' -> '-0.5'
    procedure AddFloatStr(P: PUTF8Char);
{$ifndef DELPHI5OROLDER} { array of const is buggy in Delphi 5 :( }
    /// append strings or integers with a specified format
    // - % = #37 indicates a string, integer, floating-point, or class parameter
    // to be appended as text (e.g. class name)
    // - $ = #36 indicates an integer to be written with 2 digits and a comma
    // - Ј = #163 indicates an integer to be written with 4 digits and a comma
    // - µ = #181 indicates an integer to be written with 3 digits without any comma
    // - ¤ = #164 indicates CR+LF chars
    // - CR = #13 indicates CR+LF chars
    // - § = #167 indicates to trim last comma
    // - since some of this characters above are > #127, they are not UTF-8
    // ready, so we expect the input format to be WinAnsi, i.e. mostly English
    // text (with chars < #128) with some values to be inserted inside
    // - if StringEscape is false (by default), the text won't be escaped before
    // adding; but if set to true text will be JSON escaped at writing
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Add(Format: PWinAnsiChar; const Values: array of const;
      Escape: TTextWriterKind=twNone); overload;
{$endif DELPHI5OROLDER}
    /// append CR+LF chars
    procedure AddCR;
      {$ifdef HASINLINE}inline;{$endif}
    /// write the same character multiple times
    procedure AddChars(aChar: AnsiChar; aCount: integer);
    /// append an Integer Value as a 2 digits String with comma
    procedure Add2(Value: integer);
    /// append the current date and time, in a log-friendly format
    // - e.g. append '20110325 19241502 '
    // - this method is very fast, and avoid most calculation or API calls
    procedure AddCurrentLogTime;
    /// append a time period, specified in micro seconds
    procedure AddMicroSec(MS: cardinal);
    /// append an Integer Value as a 4 digits String with comma
    procedure Add4(Value: integer);
    /// append an Integer Value as a 3 digits String without any added comma
    procedure Add3(Value: integer);
    /// append a line of text with CR+LF at the end
    procedure AddLine(const Text: shortstring);
    /// append a String
    procedure AddString(const Text: RawUTF8);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a ShortString
    procedure AddShort(const Text: ShortString);
      {$ifdef HASINLINE}inline;{$endif}
    /// append a ShortString property name, as '"PropName":'
    procedure AddPropName(const PropName: ShortString);
    /// append a RawUTF8 property name, as '"FieldName":'
    procedure AddFieldName(const FieldName: RawUTF8);
    /// append the class name of an Object instance as text
    // - aClass must be not nil
    procedure AddClassName(aClass: TClass);
    /// append an Instance name and pointer, as '"TObjectList(00425E68)"'+SepChar
    // - Instance must be not nil
    procedure AddInstanceName(Instance: TObject; SepChar: AnsiChar);
    /// append an Instance name and pointer, as 'TObjectList(00425E68)'+SepChar
    // - Instance must be not nil
    procedure AddInstancePointer(Instance: TObject; SepChar: AnsiChar);
    /// append an array of integers as CSV
    procedure AddCSV(const Integers: array of Integer); overload;
    /// append an array of doubles as CSV
    procedure AddCSV(const Doubles: array of double); overload;
    /// append an array of RawUTF8 as CSV
    procedure AddCSV(const Values: array of RawUTF8); overload;
    /// write some data as hexa chars
    procedure WrHex(P: PAnsiChar; Len: integer);
    /// write some data Base64 encoded
    // - if withMagic is TRUE, will write as '"\uFFF0base64encodedbinary"'
    procedure WrBase64(P: PAnsiChar; Len: cardinal; withMagic: boolean);
    /// write some record content as binary, Base64 encoded with our magic prefix
    procedure WrRecord(const Rec; TypeInfo: pointer);
    /// write some #0 ended UTF-8 text, according to the specified format
    procedure Add(P: PUTF8Char; Escape: TTextWriterKind); overload;
    /// write some #0 ended UTF-8 text, according to the specified format
    procedure Add(P: PUTF8Char; Len: PtrInt; Escape: TTextWriterKind); overload;
    /// write some #0 ended Unicode text as UTF-8, according to the specified format
    procedure AddW(P: PWord; Len: PtrInt; Escape: TTextWriterKind);
    /// append some chars to the buffer
    // - if Len is 0, Len is calculated from zero-ended char
    // - don't escapes chars according to the JSON RFC
    procedure AddNoJSONEscape(P: Pointer; Len: integer=0);
    /// append some binary data as hexadecimal text conversion
    procedure AddBinToHex(P: Pointer; Len: integer);
    /// fast conversion from binary data into hexa chars, ready to be displayed
    // - using this function with Bin^ as an integer value will encode it
    // in big-endian order (most-signignifican byte first): use it for display
    // - up to 128 bytes may be converted 
    procedure AddBinToHexDisplay(Bin: pointer; BinBytes: integer);
    /// add the pointer into hexa chars, ready to be displayed
    procedure AddPointer(P: PtrUInt);
    /// append some unicode chars to the buffer
    // - WideCharCount is the unicode chars count, not the byte size
    // - don't escapes chars according to the JSON RFC
    // - will convert the Unicode chars into UTF-8
    procedure AddNoJSONEscapeW(P: PWord; WideCharCount: integer);
    /// append some UTF-8 encoded chars to the buffer
    // - if Len is 0, Len is calculated from zero-ended char
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscape(P: Pointer; Len: PtrInt=0); overload;
    /// append some UTF-8 encoded chars to the buffer, from a generic string type
    // - faster than AddJSONEscape(pointer(StringToUTF8(string))
    // - if Len is 0, Len is calculated from zero-ended char
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeString(const s: string);
      {$ifdef UNICODE}inline;{$endif}
    /// append some UTF-8 encoded chars to the buffer, from a generic string type
    // - faster than AddNoJSONEscape(pointer(StringToUTF8(string))
    // - don't escapes chars according to the JSON RFC
    // - will convert the Unicode chars into UTF-8
    procedure AddNoJSONEscapeString(const s: string);
      {$ifdef UNICODE}inline;{$endif}
    /// append some Unicode encoded chars to the buffer
    // - if Len is 0, Len is calculated from zero-ended widechar
    // - escapes chars according to the JSON RFC
    procedure AddJSONEscapeW(P: PWord; Len: PtrInt=0);
    /// append an open array constant value to the buffer
    // - "" will be added if necessary
    // - escapes chars according to the JSON RFC
    // - very fast (avoid most temporary storage)
    procedure AddJSONEscape(const V: TVarRec); overload;
    /// encode the supplied data as an UTF-8 valid JSON object content
    // - data must be supplied two by two, as Name,Value pairs, e.g.
    // ! aWriter.AddJSONEscape(['name','John','year',1972])
    // will append to the buffer:
    // ! '{"name":"John","year":1972}'
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly) 
    procedure AddJSONEscape(const NameValuePairs: array of const); overload;
    /// append a dynamic array content as UTF-8 encoded JSON array
    // - expect a dynamic array TDynArray wrapper as incoming parameter
    // - TIntegerDynArray, TInt64DynArray, TCardinalDynArray, TDoubleDynArray,
    // TCurrencyDynArray, TWordDynArray and TByteDynArray will be written as
    // numerical JSON values
    // - TRawUTF8DynArray, TWinAnsiDynArray, TRawByteStringDynArray,
    // TStringDynArray, TWideStringDynArray, TSynUnicodeDynArray, TTimeLogDynArray,
    // and TDateTimeDynArray will be written as escaped UTF-8 JSON strings
    // (and Iso-8601 textual encoding if necessary)
    // - you can add some custom serializers via RegisterCustomJSONSerializer()
    // class method, to serialize any dynamic array as valid JSON
    // - any other non-standard or non-registered kind of dynamic array (including
    // array of records) will be written as Base64 encoded binary stream, with a
    // JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code) - this will
    // include TBytes (i.e. array of bytes) content, which is a good candidate
    // for BLOB stream
    // - typical content could be
    // ! '[1,2,3,4]' or '["\uFFF0base64encodedbinary"]'
    procedure AddDynArrayJSON(const DynArray: TDynArray);
    /// append a dynamic array content as UTF-8 encoded JSON
    // - default serialization will use Base64 encoded binary stream, or
    // a custom serialization, in case of a previous registration via
    // RegisterCustomJSONSerializer() class method - from a dynamic array
    // handling this kind of records, or directly from TypeInfo() of the record
    procedure AddRecordJSON(const Rec; TypeInfo: pointer);
    /// define a custom serialization for a given dynamic array or record
    // - expects TypeInfo() from a dynamic array or a record (will raise an
    // exception otherwise)
    // - for a dynamic array, the associated item record RTTI will be registered
    // - for a record, any matching dynamic array will also be registered
    // - by default, TIntegerDynArray and such known classes are processed as
    // true JSON arrays: but you can specify here some callbacks to perform
    // the serialization process for any kind of dynamic array
    // - any previous registration is overriden
    // - setting both aReader=aWriter=nil will return back to the default
    // binary + Base64 encoding serialization (i.e. undefine custom serializer)
    class procedure RegisterCustomJSONSerializer(aTypeInfo: pointer;
      aReader: TDynArrayJSONCustomReader; aWriter: TDynArrayJSONCustomWriter);
    /// append some chars to the buffer in one line
    // - P should be ended with a #0
    // - will write #1..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUTF8Char); overload;
    /// append some chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLine(P: PUTF8Char; Len: PtrInt); overload;
    /// append some wide chars to the buffer in one line
    // - will write #0..#31 chars as spaces (so content will stay on the same line)
    procedure AddOnSameLineW(P: PWord; Len: PtrInt); 
    /// serialize as JSON the given object
    // - this default implementation will write null, or only write the
    // class name and pointer if FullExpand is true - use TJSONSerializer.
    // WriteObject method for full RTTI handling
    // - default implementation will write TList/TCollection/TStrings/TRawUTF8List
    // as appropriate array of class name/pointer (if FullExpand=true) or string
    procedure WriteObject(Value: TObject; HumanReadable: boolean=false;
      DontStoreDefault: boolean=true; FullExpand: boolean=false); virtual;
    /// the last char appended is canceled
    procedure CancelLastChar;
      {$ifdef HASINLINE}inline;{$endif}
    /// the last char appended is canceled if it was a ','
    procedure CancelLastComma;
      {$ifdef HASINLINE}inline;{$endif}
    /// rewind the Stream to the position when Create() was called
    procedure CancelAll;
    /// count of add byte to the stream
    property TextLength: integer read GetLength;
    /// the internal TStream used for storage
    // - you should call the Flush method before using this TStream content,
    // to flush all pending characters to the stream
    property Stream: TStream read fStream write SetStream;
  end;

  /// simple writer to a Stream, specialized for the JSON format and SQL export
  // - use an internal buffer, faster than string+string
  TJSONWriter = class(TTextWriter)
  protected
    /// used to store output format
    fExpand: boolean;
    /// used to store output format for TSQLRecord.GetJSONValues()
    fWithID: boolean;
    /// used to store field for TSQLRecord.GetJSONValues()
    fFields: TSQLFieldBits;
    fFieldMax: integer;
    /// if not Expanded format, contains the Stream position of the first
    // usefull Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    fStartDataPosition: integer;
  public
    /// used internally to store column names and count for AddColumns
    ColNames: TRawUTF8DynArray;
    /// the data will be written to the specified Stream
    // - if no Stream is supplied, a temporary memory stream will be created
    // (it's faster to supply one, e.g. any TSQLRest.TempMemoryStream)
    constructor Create(aStream: TStream; Expand, withID: boolean;
      const Fields: TSQLFieldBits=[]); overload;
    /// rewind the Stream position and write void JSON object
    procedure CancelAllVoid;
    /// write or init field names for appropriate JSON Expand later use
    // - ColNames[] must have been initialized before calling this procedure
    // - if aKnownRowsCount is not null, a "rowCount":... item will be added
    // to the generated JSON stream (for faster unserialization of huge content)
    procedure AddColumns(aKnownRowsCount: integer=0);
    /// the first data row is erased from the content
    // - only works if the associated storage stream is TMemoryStream
    // - expect not Expanded format
    procedure TrimFirstRow;
    /// is set to TRUE in case of Expanded format
    property Expand: boolean read fExpand write fExpand;
    /// is set to TRUE if the ID field must be appended to the resulting JSON
    property WithID: boolean read fWithID write fWithID;
    /// Read-Only access to the field bits set for each column to be stored
    property Fields: TSQLFieldBits read fFields write fFields;
    /// Read-Only access to the higher field index to be stored
    // - i.e. the highest bit set in Fields
    property FieldMax: integer read fFieldMax write fFieldMax;
    /// if not Expanded format, contains the Stream position of the first
    // usefull Row of data; i.e. ',val11' position in:
    // & { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
    property StartDataPosition: integer read fStartDataPosition;
  end;

  /// implement a cache of some key/value pairs, e.g. to improve reading speed
  // - used e.g. by TSQLDataBase for caching the SELECT statements results in an
  // internal JSON format (which is faster than a query to the SQLite3 engine)
  // - internally make use of an efficient hashing algorithm for fast response
  // (i.e. TSynNameValue will use the TDynArrayHashed wrapper mechanism)
  TSynCache = class
  protected
    /// last index in fNameValue.List[] if was added by Find()
    fFindLastAddedIndex: integer;
    /// store Key/Value pairs
    fNameValue: TSynNameValue;
    /// the global size of Values in cache, in bytes (to prevent memory burn)
    fValueSize: cardinal;
    /// the maximum RAM to be used for values, in bytes
    fMaxCacheRamUsed: cardinal;
  public
    /// initialize the internal storage
    // - aMaxCacheRamUsed can set the maximum RAM to be used for values, in bytes
    // (default is 16 MB per cache): cache will be reset when so much value
    // will be reached
    constructor Create(aMaxCacheRamUsed: cardinal=16384*1024);
    /// find a Key in the cache entries
    // - return '' if nothing found
    // - return the associated Value otherwize, and the associated integer tag
    // if aResultTag address is supplied
    function Find(const aKey: RawUTF8; aResultTag: PPtrInt): RawUTF8;
    /// add a Key and its associated value (and tag) to the cache entries
    // - you MUST always call Find() with the associated Key first
    procedure Add(const aValue: RawUTF8; aTag: PtrInt);
    /// called after a write access to the database to flush the cache
    // - set Count to 0
    // - release all cache memory
    // - returns TRUE if was flushed, i.e. if there was something in cache
    function Reset: boolean;
    /// number of entries in the cache
    property Count: integer read fNameValue.Count;
  end;

  TObjectListHashedAbstract = class
  protected
    fList: TObjectDynArray;
    fCount: integer;
    fHash: TDynArrayHashed;
    fFreeItems: boolean;
    fHashValid: boolean;
    fHashed: boolean;
  public
    /// initialize the class instance
    // - if aFreeItems is TRUE (default), will behave like a TObjectList
    // - if aFreeItems is FALSE, will behave like a TList
    constructor Create(aFreeItems: boolean=true); reintroduce;
    /// release used memory
    destructor Destroy; override;
    /// search and add an object reference to the list
    // - returns the found/added index
    function Add(aObject: TObject; out wasAdded: boolean): integer; virtual; abstract;
    /// retrieve an object index within the list, using a fast hash table
    // - returns -1 if not found
    function IndexOf(aObject: TObject): integer; virtual; abstract;
    /// delete an object from the list
    procedure Delete(aIndex: integer); overload;
    /// delete an object from the list
    procedure Delete(aObject: TObject); overload;
    /// direct access to the items list array
    property List: TObjectDynArray read fList;
    /// returns the count of stored objects
    property Count: integer read fCount;
    /// direct access to the underlying hashing engine
    property Hash: TDynArrayHashed read fHash;
  end;

  /// this class behaves like TList/TObjectList, but will use hashing
  // for (much) faster IndexOf() method
  TObjectListHashed = class(TObjectListHashedAbstract)
  public
    /// search and add an object reference to the list
    // - returns the found/added index
    // - if added, hash is stored and Items[] := aObject 
    function Add(aObject: TObject; out wasAdded: boolean): integer; override;
    /// retrieve an object index within the list, using a fast hash table
    // - returns -1 if not found
    function IndexOf(aObject: TObject): integer; override;
  end;

  /// function prototype used to retrieve the hashed property of a
  // TObjectListPropertyHashed list
  TObjectListPropertyHashedAccessProp = function(aObject: TObject): pointer;

  /// this class will hash and search for a sub property of the stored objects
  TObjectListPropertyHashed = class(TObjectListHashedAbstract)
  protected
    fSubPropAccess: TObjectListPropertyHashedAccessProp;
    function IntHash(const Elem): cardinal;
    function IntComp(const A,B): integer;
    procedure IntHashValid;
  public
    /// initialize the class instance with the corresponding callback in order
    // to handle sub-property hashing and search
    // - see TSetWeakZeroClass in SQLite3Commons unit as example:
    // !  function WeakZeroClassSubProp(aObject: TObject): TObject;
    // !  begin
    // !    result := TSetWeakZeroInstance(aObject).fInstance;
    // !  end;
    // - by default, aHashElement/aCompare will hash/search for pointers:
    // you can specify the hash/search methods according to your sub property
    // (e.g. HashAnsiStringI/SortDynArrayAnsiStringI for a RawUTF8)
    // - if aFreeItems is TRUE (default), will behave like a TObjectList;
    // if aFreeItems is FALSE, will behave like a TList
    constructor Create(aSubPropAccess: TObjectListPropertyHashedAccessProp;
      aHashElement: TDynArrayHashOne=nil; aCompare: TDynArraySortCompare=nil;
      aFreeItems: boolean=true); reintroduce;
    /// search and add an object reference to the list
    // - returns the found/added index
    // - if added, only the hash is stored: caller has to set List[i] 
    function Add(aObject: TObject; out wasAdded: boolean): integer; override;
    /// retrieve an object index within the list, using a fast hash table
    // - returns -1 if not found
    function IndexOf(aObject: TObject): integer; override;
  end;


  /// This class is able to emulate a TStringList with our native UTF-8 string type
  // - cross-compiler, from Delphi 6 up to XE2, i.e is Unicode Ready
  TRawUTF8List = class
  private
    fOnChange, fOnChangeHidden: TNotifyEvent;
    fOnChangeTrigerred: boolean;
    fOnChangeLevel: PtrInt;
    procedure OnChangeHidden(Sender: TObject);
    procedure SetCapacity(const Value: PtrInt);
    function GetCapacity: PtrInt;
    procedure Put(Index: PtrInt; const Value: RawUTF8);
    function GetCount: PtrInt;
    procedure PutObject(Index: PtrInt; const Value: TObject);
    function GetName(Index: PtrInt): RawUTF8;
    function GetValue(const Name: RawUTF8): RawUTF8;
    procedure SetValue(const Name, Value: RawUTF8);
    function GetTextCRLF: RawUTF8;
    procedure SetTextCRLF(const Value: RawUTF8);
    procedure SetTextPtr(P: PUTF8Char; const Delimiter: RawUTF8);
    function GetListPtr: PPUtf8CharArray;
    function GetObjectPtr: PPointerArray;
  protected
    fCount: PtrInt;
    fCapacity: PtrInt;
    fList: TRawUTF8DynArray;
    fObjects: TObjectDynArray;
    fNameValueSep: AnsiChar;
    fUseHashing: boolean;
    fHash: TDynArrayHashed;
    procedure Changed; virtual;
  public
    /// initialize the class instance
    constructor Create;
    /// get a stored RawUTF8 item
    // - returns '' and raise no exception in case of out of range supplied index
    function Get(Index: PtrInt): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    /// get a stored Object item
    // - returns nil and raise no exception in case of out of range supplied index
    function GetObject(Index: PtrInt): TObject; {$ifdef HASINLINE}inline;{$endif}
    /// store a new RawUTF8 item
    // - returns -1 and raise no exception in case of self=nil
    function Add(const aText: RawUTF8): PtrInt; {$ifdef HASINLINE}inline;{$endif}
    /// store a new RawUTF8 item, and its associated TObject
    // - returns -1 and raise no exception in case of self=nil
    function AddObject(const aText: RawUTF8; aObject: TObject): PtrInt;
    /// append a specified list to the current content
    procedure AddRawUTF8List(List: TRawUTF8List);
    /// delete a stored RawUTF8 item, and its associated TObject
    // - raise no exception in case of out of range supplied index
    procedure Delete(Index: PtrInt);
    /// delete a stored RawUTF8 item, and its associated TObject, from
    // a given Name when stored as 'Name=Value' pairs
    // - raise no exception in case of out of range supplied index
    function DeleteFromName(const Name: RawUTF8): PtrInt;
    /// update Value from an existing Name=Value, then optinally delete the entry
    procedure UpdateValue(const Name: RawUTF8; var Value: RawUTF8; ThenDelete: boolean);
    /// erase all stored RawUTF8 items
    procedure Clear;
    /// find a RawUTF8 item in the stored Strings[] list
    // - this search is case sensitive
    function IndexOf(const aText: RawUTF8): PtrInt; virtual;
    /// find the index of a given Name when stored as 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    function IndexOfName(const Name: RawUTF8): PtrInt;
    /// find a TObject item index in the stored Objects[] list
    function IndexOfObject(aObject: TObject): PtrInt;
    /// access to the Value of a given 'Name=Value' pair
    function GetValueAt(Index: PtrInt): RawUTF8;
    /// retrieve the all lines, separated by the supplied delimiter
    function GetText(const Delimiter: RawUTF8=#13#10): RawUTF8;
    /// the OnChange event will be raised only when EndUpdate will be called
    procedure BeginUpdate;
    /// call the OnChange event if changes occured
    procedure EndUpdate;
    /// set all lines, separated by the supplied delimiter
    procedure SetText(const aText: RawUTF8; const Delimiter: RawUTF8=#13#10);
    /// set all lines from an UTF-8 text file
    // - expect the file is explicitly an UTF-8 file
    // - will ignore any trailing UTF-8 BOM in the file content, but will not
    // expect one either
    procedure LoadFromFile(const FileName: TFileName);
    /// return the count of stored RawUTF8
    property Count: PtrInt read GetCount;
    /// set or retrive the current memory capacity of the RawUTF8 list
    property Capacity: PtrInt read GetCapacity write SetCapacity;
    /// get or set a RawUTF8 item
    // - returns '' and raise no exception in case of out of range supplied index
    property Strings[Index: PtrInt]: RawUTF8 read Get write Put; default;
    /// get or set a Object item
    // - returns nil and raise no exception in case of out of range supplied index
    property Objects[Index: PtrInt]: TObject read GetObject write PutObject;
    /// retrieve the corresponding Name when stored as 'Name=Value' pairs
    property Names[Index: PtrInt]: RawUTF8 read GetName;
    /// access to the corresponding 'Name=Value' pairs
    // - search on Name is case-insensitive with 'Name=Value' pairs
    property Values[const Name: RawUTF8]: RawUTF8 read GetValue write SetValue;
    /// the char separator between 'Name=Value' pairs
    // - equals '=' by default
    property NameValueSep: AnsiChar read fNameValueSep write fNameValueSep;
    /// set or retrieve all items as text lines
    // - lines are separated by #13#10 (CRLF) by default; use GetText and
    // SetText methods if you want to use another line delimiter (even a comma)
    property Text: RawUTF8 read GetTextCRLF write SetTextCRLF;
    /// Event trigerred when an entry is modified
    property OnChange: TNotifyEvent read fOnChange write fOnChange;
    /// direct access to the memory of the RawUTF8 array
    property ListPtr: PPUtf8CharArray read GetListPtr;
    /// direct access to the memory of the Objects array
    property ObjectPtr: PPointerArray read GetObjectPtr;
  end; 

  /// a TRawUTF8List which will use an internal hash table for faster IndexOf()
  // - this is a rather rough implementation: all values are re-hashed after
  // change: but purpose of this class is to allow faster access of a static
  // list of identifiers
  TRawUTF8ListHashed = class(TRawUTF8List)
  protected
    fHash: TDynArrayHashed;
    fChanged, fHashCaseInsensitive: boolean;
    procedure SetHashCaseInsensitive(Value: boolean);
    procedure Changed; override;
  public
    /// initialize the class instance
    constructor Create;
    /// find a RawUTF8 item in the stored Strings[] list
    // - this overriden method will update the internal hash table (if needed),
    // then use it to retrieve the corresponding matching index
    function IndexOf(const aText: RawUTF8): PtrInt; override;
    /// specify if the IndexOf() hashed search is case sensitive or not
    // - is FALSE by default, as for the default TRawUTF8List.IndexOf() method
    property CaseInsensitive: boolean read fHashCaseInsensitive write SetHashCaseInsensitive;
  end;

  /// handle memory mapping of a file content
  /// used to store and retrieve Words in a sorted array
  TMemoryMap = {$ifndef UNICODE}object{$else}record{$endif}
  private
    fBuf: PAnsiChar;
    fBufSize: cardinal;
    fFile: THandle;
    fMap: THandle;
    fFileSize: Int64;
    fFileLocal: boolean;
  public
    /// map the corresponding file handle
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    function Map(aFile: THandle; aCustomSize: cardinal=0; aCustomOffset: Int64=0): boolean; overload;
    /// map the file specified by its name
    // - file will be closed when UnMap will be called
    function Map(const aFileName: TFileName): boolean; overload;
    /// set a fixed buffer for the content
    // - emulated a memory-mapping from an existing buffer
    procedure Map(aBuffer: pointer; aBufferSize: cardinal); overload;
    /// unmap the file
    procedure UnMap;
    /// retrieve the memory buffer mapped to the file content
    property Buffer: PAnsiChar read fBuf;
    /// retrieve the buffer size
    property Size: cardinal read fBufSize;
  end;

{$M+}
  /// able to read a UTF-8 text file using memory map
  // - much faster than TStringList.LoadFromFile()
  // - will ignore any trailing UTF-8 BOM in the file content, but will not
  // expect one either
  TMemoryMapText = class
  protected
    fLines: PPointerArray;
    fCount: integer;
    fMapEnd: PUTF8Char;
    fMap: TMemoryMap;
    fFileName: TFileName;
    function GetLine(aIndex: integer): RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    function GetString(aIndex: integer): string; {$ifdef HASINLINE}inline;{$endif}
    /// call once by Create constructors when fMap has been initialized
    procedure LoadFromMap; virtual;
    /// call once per line, from LoadFromMap method
    // - default implementation will set  fLines[fCount] := LineBeg;
    // - override this method to add some per-line process at loading: it will
    // avoid reading the entire file more than once
    procedure ProcessOneLine(LineBeg, LineEnd: PUTF8Char; max: Integer); virtual;
  public
    /// read an UTF-8 encoded text file
    // - every line beginning is stored into LinePointers[]
    constructor Create(const aFileName: TFileName); overload;
    /// read an UTF-8 encoded text file content
    // - every line beginning is stored into LinePointers[]
    // - this overloaded constructor accept an existing memory buffer (some
    // uncompressed data e.g.)
    constructor Create(aFileContent: PUTF8Char; aFileSize: integer); overload;
    /// release the memory map and internal LinePointers[]
    destructor Destroy; override;
    /// retrieve the number of UTF-8 chars of the given line
    // - warning: no range check is performed about supplied index
    function LineSize(aIndex: integer): integer;
      {$ifdef HASINLINE}inline;{$endif}
    /// check if there is at least a given number of UTF-8 chars in the given line
    // - this is faster than LineSize(aIndex)<aMinimalCount for big lines
    function LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
      {$ifdef HASINLINE}inline;{$endif}
    /// returns TRUE if the supplied text is contained in the corresponding line
    function LineContains(const aUpperSearch: RawUTF8; aIndex: Integer): Boolean;
    /// retrieve a line content as UTF-8
    // - a temporary UTF-8 string is created
    // - will return '' if aIndex is out of range
    property Lines[aIndex: integer]: RawUTF8 read GetLine;
    /// retrieve a line content as generic VCL string type
    // - a temporary VCL string is created (after conversion for UNICODE Delphi)
    // - will return '' if aIndex is out of range
    property Strings[aIndex: integer]: string read GetString;
    /// direct access to each text line
    // - use LineSize() method to retrieve line length, since end of line will
    // NOT end with #0, but with #13 or #10
    // - warning: no range check is performed about supplied index
    property LinePointers: PPointerArray read fLines;
    /// the memory map used to access the raw file content
    property Map: TMemoryMap read fMap;
  published
    /// the file name which was opened by this instance
    property FileName: TFileName read fFileName write fFileName;
    /// the number of text lines
    property Count: integer read fCount;
  end;
{$M-}

  /// a TStream using a RawByteString as internal storage
  // - default TStringStream uses WideChars since Delphi 2009, so it is
  // not compatible with previous versions, and it does make sense to
  // work with RawByteString in our UTF-8 oriented framework
  TRawByteStringStream = class(TStream)
  private
    fDataString: RawByteString;
    fPosition: Integer;
  protected
    procedure SetSize(NewSize: Longint); override;
  public
    constructor Create(const aString: RawByteString=''); overload;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(Offset: Longint; Origin: Word): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    property DataString: RawByteString read fDataString write fDataString;
  end;

  /// a TStream pointing to some in-memory data, for instance UTF-8 text
  // - warning: there is no local copy of the supplied content: the
  // source data must be available during all the TSynMemoryStream usage
  TSynMemoryStream = class(TCustomMemoryStream)
  public
    /// create a TStream with the supplied text data
    // - warning: there is no local copy of the supplied content: the aText
    // variable must be available during all the TSynMemoryStream usage:
    // don't release aText before calling TSynMemoryStream.Free
    // - aText can be on any AnsiString format, e.g. RawUTF8 or RawByteString
    constructor Create(const aText: RawByteString); overload;
    /// create a TStream with the supplied data buffer
    // - warning: there is no local copy of the supplied content: the
    // Data/DataLen buffer must be available during all the TSynMemoryStream usage:
    // don't release the source Data before calling TSynMemoryStream.Free
    constructor Create(Data: pointer; DataLen: integer); overload;
    /// this TStream is read-only: calling this method will raise an exception
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  /// a TStream created from a file content, using fast memory mapping
  TSynMemoryStreamMapped = class(TSynMemoryStream)
  protected
    fMap: TMemoryMap;
    fFileStream: TFileStream;
  public
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(const aFileName: TFileName;
      aCustomSize: cardinal=0; aCustomOffset: Int64=0); overload;
    /// create a TStream from a file content using fast memory mapping
    // - if aCustomSize and aCustomOffset are specified, the corresponding
    // map view if created (by default, will map whole file)
    constructor Create(aFile: THandle;
      aCustomSize: cardinal=0; aCustomOffset: Int64=0); overload;
    /// release any internal mapped file instance
    destructor Destroy; override;
  end;

  /// available kind of integer array storage, corresponding to the data layout
  // - wkUInt32 will write the content as "plain" 4 bytes binary (this is the
  // prefered way if the integers can be negative)
  // - wkVarUInt32 will write the content using our 32-bit variable-length integer
  // encoding
  // - wkVarInt32 will write the content using our 32-bit variable-length integer
  // encoding and the by-two complement (0=0,1=1,2=-1,3=2,4=-2...)
  // - wkSorted will write an increasing array of integers, handling the special
  // case of a difference of 1 between two values (therefore is very optimized
  // to store an array of IDs)
  // - wkOffsetU and wkOffsetI will write the difference between two successive
  // values, handling constant difference (Unsigned or Integer) in an optimized manner
  TFileBufferWriterKind = (wkUInt32, wkVarUInt32, wkVarInt32, wkSorted,
    wkOffsetU, wkOffsetI);

  /// this class can be used to speed up writing to a file
  // - big speed up if data is written in small blocks
  // - also handle optimized storage of any dynamic array of Integer/Int64/RawUTF8
  TFileBufferWriter = class
  private
    fPos: integer;
    fBufLen: Integer;
    fStream: TStream;
    fTotalWritten: Int64;
    fInternalStream: boolean;
    fBuf: RawByteString;
  public
    /// initialize the buffer, and specify a file handle to use for writing
    // - use an internal buffer of the specified size
    constructor Create(aFile: THandle; BufLen: integer=65536); overload;
    /// initialize the buffer, and specify a TStream to use for writing
    // - use an internal buffer of the specified size
    constructor Create(aStream: TStream; BufLen: integer=65536); overload;
    /// initialize the buffer, and specify a file to use for writing
    // - use an internal buffer of the specified size
    constructor Create(const aFileName: TFileName; BufLen: integer=65536); overload;
    /// initialize the buffer, using an internal TMemoryStream
    // - use Flush then TMemoryStream(Stream) to retrieve its content
    constructor CreateInMemoryStream;
    /// release internal TStream (after AssignToHandle call)
    destructor Destroy; override;
    /// append some data at the current position
    procedure Write(Data: pointer; DataLen: integer); overload;
    /// append some UTF-8 encoded text at the current position
    procedure Write(const Text: RawUTF8); overload;
    /// append a cardinal value using 32-bit variable-length integer encoding
    procedure WriteVarUInt32(Value: PtrUInt);
    /// append an integer value using 32-bit variable-length integer encoding of
    // the by-two complement of the given value
    procedure WriteVarInt32(Value: PtrInt);
    /// append cardinal values (NONE must be negative!) using 32-bit
    // variable-length integer encoding or other specialized algorithm,
    // depending on the data layout
    procedure WriteVarUInt32Array(const Values: TIntegerDynArray; ValuesCount: integer;
      DataLayout: TFileBufferWriterKind);
    /// append UInt64 values using 64-bit variable length integer encoding
    // - if Offset is TRUE, then it will store the difference between
    // two values using 32-bit variable-length integer encoding (in this case,
    // a fixed-sized record storage is also handled separately)
    procedure WriteVarUInt64DynArray(const Values: TInt64DynArray;
      ValuesCount: integer; Offset: Boolean);
    /// append the RawUTF8 dynamic array
    // - handled the fixed size strings array case in a very efficient way
    procedure WriteRawUTF8DynArray(const Values: TRawUTF8DynArray; ValuesCount: integer);
    /// append the RawUTF8List content
    // - if StoreObjectsAsVarUInt32 is TRUE, all Objects[] properties will be
    // stored as VarUInt32
    procedure WriteRawUTF8List(List: TRawUTF8List; StoreObjectsAsVarUInt32: Boolean=false);
    /// append a TStream content
    // - is StreamSize is left as -1, the Stream.Size is used
    // - the size of the content is stored in the resulting stream
    procedure WriteStream(aStream: TCustomMemoryStream; aStreamSize: Integer=-1);
    /// write any pending data in the internal buffer to the file
    // - after a Flush, it's possible to call FileSeek64(aFile,....)
    // - returns the number of bytes written between two FLush method calls
    function Flush: Int64;
    /// the associated writing stream
    property Stream: TStream read fStream;
    /// get the byte count written since last Flush
    property TotalWritten: Int64 read fTotalWritten;
  end;

type
  PFileBufferReader = ^TFileBufferReader;

  /// this structure can be used to speed up reading from a file
  // - use internaly memory mapped files for a file up to 2 GB (Windows has
  // problems with memory mapped files bigger than this size limit - at least
  // with 32 bit executables) - but sometimes, Windows fails to allocate
  // more than 512 MB for a memory map, because it does lack of contiguous
  // memory space: in this case, we fall back on direct file reading
  // - maximum handled file size has no limit (but will use slower direct
  // file reading) 
  // - is defined either as an object either as a record, due to a bug
  // in Delphi 2009/2010 compiler (at least): this structure is not initialized
  // if defined as an object on the stack, but will be as a record :(
  {$ifdef UNICODE}
  TFileBufferReader = record
  private
  {$else}
  TFileBufferReader = object
  protected
  {$endif}
    fCurrentPos: PtrUInt;
    fMap: TMemoryMap;
    /// get Isize + buffer from current memory map or fBufTemp into (P,PEnd)
    procedure ReadChunk(var P, PEnd: PByte; var BufTemp: RawByteString);
  public
    /// initialize the buffer, and specify a file to use for reading
    // - will try to map the whole file content in memory
    // - if memory mapping failed, methods will use default slower file API 
    procedure Open(aFile: THandle);
    /// initialize the buffer from an already existing memory block
    // - may be e.g. a resource or a TMemoryStream
    procedure OpenFrom(aBuffer: pointer; aBufferSize: cardinal); overload;
    /// initialize the buffer from an already existing Stream
    // - accept either TFileStream or TCustomMemoryStream kind of stream 
    function OpenFrom(Stream: TStream): boolean; overload;
    /// close all internal mapped files
    // - call Open() again to use the Read() methods
    procedure Close;
    {$ifndef CPU64}
    /// change the current reading position, from the beginning of the file
    // - returns TRUE if success, or FALSE if Offset is out of range
    function Seek(Offset: Int64): boolean; overload;
    {$endif}
    /// change the current reading position, from the beginning of the file
    // - returns TRUE if success, or FALSE if Offset is out of range
    function Seek(Offset: PtrInt): boolean; overload;
    /// read some bytes from the given reading position
    // - returns the number of bytes which was read
    function Read(Data: pointer; DataLen: integer): integer; overload;
    /// read some UTF-8 encoded text at the current position
    // - returns the resulting text length, in bytes
    function Read(out Text: RawUTF8): integer; overload;
    /// read some UTF-8 encoded text at the current position
    // - returns the resulting text
    function ReadRawUTF8: RawUTF8; {$ifdef HASINLINE}inline;{$endif}
    /// read one byte
    // - if reached end of file, don't raise any error, but returns 0
    function ReadByte: PtrUInt; {$ifdef HASINLINE}inline;{$endif}
    /// read one cardinal, which was written as fixed length
    // - if reached end of file, don't raise any error, but returns 0
    function ReadCardinal: cardinal;
    /// read one cardinal value encoded using our 32-bit variable-length integer
    function ReadVarUInt32: PtrUInt;
    /// read one integer value encoded using our 32-bit variable-length integer,
    // and the by-two complement
    function ReadVarInt32: PtrInt;
    /// read one UInt64 value encoded using our 64-bit variable-length integer
    function ReadVarUInt64: QWord;
    /// retrieved cardinal values encoded with TFileBufferWriter.WriteVarUInt32Array
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
    /// retrieved Int64 values encoded with TFileBufferWriter.WriteVarUInt64DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
    /// retrieved RawUTF8 values encoded with TFileBufferWriter.WriteRawUTF8DynArray
    // - returns the number of items read into Values[] (may differ from length(Values))
    function ReadVarRawUTF8DynArray(var Values: TRawUTF8DynArray): PtrInt;
    /// retrieve the RawUTF8List content encoded with TFileBufferWriter.WriteRawUTF8List
    // - if StoreObjectsAsVarUInt32 was TRUE, all Objects[] properties will be
    // retrieved as VarUInt32
    function ReadRawUTF8List(List: TRawUTF8List): boolean;
    /// retrieve a pointer to the current position, for a given data length
    // - if the data is available in the current memory mapped file, it
    // will just return a pointer to it
    // - otherwise (i.e. if the data is split between to 1GB memory map buffers),
    // data will be copied into the temporary aTempData buffer before retrieval
    function ReadPointer(DataLen: PtrUInt; var aTempData: RawByteString): pointer;
    /// create a TMemoryStream instance from the current position
    // - the content size is either specified by DataLen>=0, either available at
    // the current position, as saved by TFileBufferWriter.WriteStream method
    // - if this content fit in the current 1GB memory map buffer, a
    // TSynMemoryStream instance is returned, with no data copy (faster)
    // - if this content is not already mapped in memory, a separate memory map
    // will be created (the returned instance is a TSynMemoryStreamMapped)
    function ReadStream(DataLen: PtrInt=-1): TCustomMemoryStream;
    /// retrieve the current in-memory pointer
    // - if file was not memory-mapped, returns nil
    function CurrentMemory: pointer;
    /// raise an exception in case of invalid content
    procedure ErrorInvalidContent;
    /// read-only access to the global file size
    property FileSize: Int64 read fMap.fFileSize;
  end;


/// FileSeek() overloaded function, working with huge files
// - Delphi FileSeek() is buggy -> use this function to safe access files > 2 GB
// (thanks to sanyin for the report)
function FileSeek64(Handle: THandle; const Offset: Int64; Origin: DWORD): Int64;

/// encode the supplied data as an UTF-8 valid JSON object content
// - data must be supplied two by two, as Name,Value pairs, e.g.
// ! JSONEncode(['name','John','year',1972]) = '{"name":"John","year":1972}'
// - note that cardinal values should be type-casted to Int64() (otherwise
// the integer mapped value will be transmitted, therefore wrongly)
function JSONEncode(const NameValuePairs: array of const): RawUTF8;

/// encode the supplied RawUTF8 array data as an UTF-8 valid JSON array content
function JSONEncodeArray(const Values: array of RawUTF8): RawUTF8; overload;

/// encode the supplied integer array data as an UTF-8 valid JSON array content
function JSONEncodeArray(const Values: array of integer): RawUTF8; overload;

/// encode the supplied floating-point array data as an UTF-8 valid JSON array content
function JSONEncodeArray(const Values: array of double): RawUTF8; overload;

/// decode the supplied UTF-8 JSON content for the supplied names
// - data will be set in Values, according to the Names supplied e.g.
// ! JSONDecode(JSON,['name','year'],Values) -> Values[0]^='John'; Values[1]^='1972';
// - if any supplied name wasn't found its corresponding Values[] will be nil
// - this procedure will decode the JSON content in-memory, i.e. the PUtf8Char
// array is created inside JSON, which is therefore modified: make a private
// copy first if you want to reuse the JSON content
// - if HandleValuesAsObjectOrArray is TRUE, then this procedure will handle
// JSON arrays or objects
procedure JSONDecode(var JSON: RawUTF8;
  const Names: array of PUTF8Char; var Values: TPUtf8CharDynArray;
  HandleValuesAsObjectOrArray: Boolean=false); overload;

/// decode the supplied UTF-8 JSON content for the supplied names
// - data will be set in Values, according to the Names supplied e.g.
// ! JSONDecode(P,['name','year'],Values) -> Values[0]^='John'; Values[1]^='1972';
// - if any supplied name wasn't found its corresponding Values[] will be nil
// - this procedure will decode the JSON content in-memory, i.e. the PUtf8Char
// array is created inside P, which is therefore modified: make a private
// copy first if you want to reuse the JSON content
// - if HandleValuesAsObjectOrArray is TRUE, then this procedure will handle
// JSON arrays or objects
// - returns a pointer to the next content item in the JSON buffer
function JSONDecode(P: PUTF8Char; const Names: array of PUTF8Char;
  var Values: TPUtf8CharDynArray; HandleValuesAsObjectOrArray: Boolean=false): PUTF8Char; overload;

/// decode the supplied UTF-8 JSON content for the one supplied name
// - this procedure will decode the JSON content in-memory, so must be called
// only once with the same JSON data
function JSONDecode(var JSON: RawUTF8; const aName: RawUTF8='result';
  wasString: PBoolean=nil; HandleValuesAsObjectOrArray: Boolean=false): RawUTF8; overload;

/// decode a JSON field in an UTF-8 encoded buffer (used in TSQLTableJSON.Create)
// - this function decodes in the P^ buffer memory itself (no memory allocation
// or copy), for faster process - so take care that it's an unique string
// - PDest points to the next field to be decoded, or nil on any unexpected end
// - null is decoded as nil
// - '"strings"' are decoded as 'strings'
// - strings are JSON unescaped (and \u0123 is converted to UTF-8 chars)
// - any integer value is left as its ascii representation
// - wasString is set to true if the JSON value was a "string"
// - works for both field names or values (e.g. '"FieldName":' or 'Value,')
// - EndOfObject (if not nil) is set to the JSON value char (',' ':' or '}' e.g.)
function GetJSONField(P: PUTF8Char; out PDest: PUTF8Char;
  wasString: PBoolean=nil; EndOfObject: PUTF8Char=nil): PUTF8Char;

/// test if the supplied buffer is a "string" value or a numerical value
// (floating point or integer), according to the characters within
// - this version will recognize null/false/true as strings
// - e.g. IsString('0')=false, IsString('abc')=true, IsString('null')=true
function IsString(P: PUTF8Char): boolean;

/// test if the supplied buffer is a "string" value or a numerical value
// (floating or integer), according to the JSON encoding schema
// - this version will NOT recognize JSON null/false/true as strings
// - e.g. IsString('0')=false, IsString('abc')=true, IsString('null')=false
// - will follow the JSON definition of number, i.e. '0123' is a string (i.e.
// '0' is excluded at the begining of a number) and '123' is not a string
function IsStringJSON(P: PUTF8Char): boolean;

/// reach the positon of the next JSON field in the supplied UTF-8 buffer
function GotoNextJSONField(P: PUTF8Char; FieldCount: cardinal): PUTF8Char;

/// reach the position of the next JSON object of JSON array
// - first char is expected to be either [ either {
// - will return nil in case of parsing error or unexpected end (#0)
function GotoNextJSONObjectOrArray(P: PUTF8Char): PUTF8Char;


{ ************ filtering and validation classes and functions }

/// return TRUE if the supplied content is a valid email address
// - follows RFC 822, to validate local-part@domain email format
function IsValidEmail(P: PUTF8Char): boolean;

/// return TRUE if the supplied content is a valid IP v4 address
function IsValidIP4Address(P: PUTF8Char): boolean;

{/ return TRUE if the supplied content matchs to a grep-like pattern
  - ?	   	Matches any single characer
	- *	   	Matches any contiguous characters
	- [abc]  	Matches a or b or c at that position
	- [^abc]	Matches anything but a or b or c at that position
	- [!abc]	Matches anything but a or b or c at that position
	- [a-e]  	Matches a through e at that position
  - [abcx-z]  Matches a or b or c or x or y or or z, as does [a-cx-z]
  - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
  - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
    match 'this as a test' nor 'this is a zest'
  - initial C version by Kevin Boylan, first Delphi port by Sergey Seroukhov }
function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean=false): boolean;


type
  /// will define a filter or a validation process to be applied to
  // a database Record content (typicaly a TSQLRecord)
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilterOrValidate = class
  protected
    fParameters: RawUTF8;
    /// children must override this method in order to parse the JSON-encoded
    // parameters, and store it in protected field values
    // - the RawUTF8 param is not set as const, since it will probably be
    // decoded via JSONDecode(), so a local copy is needed
    procedure SetParameters(Value: RawUTF8); virtual;
  public
    /// initialize the filter or validation instance
    constructor Create(const aParameters: RawUTF8='');
    /// the optional associated parameters, supplied as JSON-encoded
    property Parameters: RawUTF8 read fParameters write SetParameters;
  end;

  /// will define a validation to be applied to a Record (typicaly a TSQLRecord)
  // field content
  // - a typical usage is to validate an email or IP adress e.g.
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynValidate = class(TSynFilterOrValidate)
  public
    /// perform the validation action to the specified value
    // - the value is expected by be UTF-8 text, as generated by
    // TPropInfo.GetValue e.g.
    // - if the validation failed, must return FALSE and put some message in
    // ErrorMsg (translated into the current language: you could e.g. use
    // a resourcestring and a SysUtils.Format() call for automatic translation
    // via the SQLite3i18n unit - you can leave ErrorMsg='' to trigger a
    // generic error message from clas name ('"Validate email" rule failed'
    // for TSynValidateEmail class e.g.)
    // - if the validation passed, will return TRUE
    function Process(FieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean;
      virtual; abstract;
  end;

  /// IP v4 address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - this versions expect no parameter
  TSynValidateIPAddress = class(TSynValidate)
  protected
  public
    /// perform the IP Address validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  /// IP address validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - optional JSON encoded parameters are "AllowedTLD" or "ForbiddenTLD",
  // expecting a CSV lis of Top-Level-Domain (TLD) names, e.g.
  // $ '{"AllowedTLD":"com,org,net","ForbiddenTLD":"fr"}'
  // - this will process a validation according to RFC 822 (calling the
  // IsValidEmail() function) then will check for the TLD to be in one of
  // the Top-Level domains ('.com' and such) or a two-char country, and
  // then will check the TLD according to AllowedTLD and ForbiddenTLD
  TSynValidateEmail = class(TSynValidate)
  private
    fAllowedTLD: RawUTF8;
    fForbiddenTLD: RawUTF8;
    fForbiddenDomains: RawUTF8;
  protected
    /// decode "AlowedTLD" and/or "PreventTLD" parameters
    procedure SetParameters(Value: RawUTF8); override;
  public
    /// perform the Email Address validation action to the specified value
    // - call IsValidEmail() function and check for the supplied TLD
    function Process(aFieldIndex: integer; const Value: RawUTF8; var ErrorMsg: string): boolean; override;
    /// a CSV list of allowed TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'com,org,net'
    property AllowedTLD: RawUTF8 read fAllowedTLD write fAllowedTLD;
    /// a CSV list of forbidden TLD
    // - if accessed directly, should be set as lower case values
    // - e.g. 'fr'
    property ForbiddenTLD: RawUTF8 read fForbiddenTLD write fForbiddenTLD;
    /// a CSV list of forbidden domain names
    // - if accessed directly, should be set as lower case values
    // - not only the TLD, but whole domains like 'cracks.ru,hotmail.com' or such
    property ForbiddenDomains: RawUTF8 read fForbiddenDomains write fForbiddenDomains;
  end;

  {/ grep-like case-sensitive pattern validation of a Record field content
     (typicaly a TSQLRecord)
    - parameter is NOT JSON encoded, but is some basic grep-like pattern
    - ?	   	Matches any single characer
    - *	   	Matches any contiguous characters
    - [abc]  	Matches a or b or c at that position
    - [^abc]	Matches anything but a or b or c at that position
    - [!abc]	Matches anything but a or b or c at that position
    - [a-e]  	Matches a through e at that position
    - [abcx-z]  Matches a or b or c or x or y or or z, as does [a-cx-z]
    - 'ma?ch.*'	would match match.exe, mavch.dat, march.on, etc..
    - 'this [e-n]s a [!zy]est' would match 'this is a test', but would not
      match 'this as a test' nor 'this is a zest'
    - pattern check IS case sensitive (TSynValidatePatternI is not)
    - this class is not as complete as PCRE regex for example,
      but code overhead is very small }
  TSynValidatePattern = class(TSynValidate)
  public
    /// perform the pattern validation to the specified value
    // - pattern can be e.g. '[0-9][0-9]:[0-9][0-9]:[0-9][0-9]'
    // - this method will implement both TSynValidatePattern and
    // TSynValidatePatternI, checking the current class
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
  end;

  {/ grep-like case-insensitive pattern validation of a Record field content
    (typicaly a TSQLRecord)
    - parameter is NOT JSON encoded, but is some basic grep-like pattern
    - same as TSynValidatePattern, but is NOT case sensitive }
  TSynValidatePatternI = class(TSynValidatePattern);

  /// text validation to be applied to a Record field content
  // (typicaly a TSQLRecord)
  // - expects optional JSON parameters of the allowed text length range as
  // $ '{"MinLength":5,"MaxLength":10,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1}
  // - default MinLength value is 1, MaxLength is maxInt: so you can specify a
  // blank TSynValidateText to avoid any void textual field
  // - MinAlphaCount, MinDigitCount, MinPunctCount, MinLowerCount and
  // MinUpperCount allow you to specify the minimal count of respectively
  // alphabetical [a-zA-Z], digit [0-9], punctuation [_!;.,/:?%$="#@(){}+-*],
  // lower case or upper case characters
  TSynValidateText = class(TSynValidate)
  private
    /// used to store internal the associated validation properties
    fProps: array[0..10] of cardinal;
  protected
    /// use sInvalidTextChar resourcestring to create a translated error message
    function ErrorMsg(fPropsIndex, InvalidTextIndex, MainIndex: integer): string;
    /// decode "MinLength", "MaxLength", and other parameters into fProps[]
    procedure SetParameters(Value: RawUTF8); override;
  public
    /// initialize the validation instance
    constructor Create(const aParameters: RawUTF8='');
    /// perform the text length validation action to the specified value
    function Process(aFieldIndex: integer; const Value: RawUTF8;
      var ErrorMsg: string): boolean; override;
    /// Minimal length value allowed for the text content
    // - the length is calculated with Unicode glyphs, not with UTF-8 encoded
    // char count
    // - default is 1, i.e. a void text will not pass the validation
    property MinLength: cardinal read fProps[0] write fProps[0];
    /// Maximal length value allowed for the text content
    // - the length is calculated with Unicode glyphs, not with UTF-8 encoded
    // char count
    // - default is maxInt, i.e. no maximum length is set
    property MaxLength: cardinal read fProps[1] write fProps[1];
    /// Minimal alphabetical character [a-zA-Z] count
    // - default is 0, i.e. no minimum set
    property MinAlphaCount: cardinal read fProps[2] write fProps[2];
    /// Minimal digit character [0-9] count
    // - default is 0, i.e. no minimum set
    property MinDigitCount: cardinal read fProps[3] write fProps[3];
    /// Minimal punctuation sign [_!;.,/:?%$="#@(){}+-*] count
    // - default is 0, i.e. no minimum set
    property MinPunctCount: cardinal read fProps[4] write fProps[4];
    /// Minimal alphabetical lower case character [a-z] count
    // - default is 0, i.e. no minimum set
    property MinLowerCount: cardinal read fProps[5] write fProps[5];
    /// Minimal alphabetical upper case character [A-Z] count
    // - default is 0, i.e. no minimum set
    property MinUpperCount: cardinal read fProps[6] write fProps[6];
    /// Minimal space count inside the value text
    // - default is 0, i.e. any space number allowed
    property MinSpaceCount: cardinal read fProps[7] write fProps[7];
    /// Maximal space count inside the value text
    // - default is maxInt, i.e. any space number allowed
    property MaxSpaceCount: cardinal read fProps[8] write fProps[8];
    /// Maximal space count allowed on the Left side
    // - default is maxInt, i.e. any Left space allowed
    property MaxLeftTrimCount: cardinal read fProps[9] write fProps[9];
    /// Maximal space count allowed on the Right side
    // - default is maxInt, i.e. any Right space allowed
    property MaxRightTrimCount: cardinal read fProps[10] write fProps[10];
  end;

  /// strong password validation for a Record field content (typicaly a TSQLRecord)
  // - the following parameters are set by default to
  // $ '{"MinLength":5,"MaxLength":10,"MinAlphaCount":1,"MinDigitCount":1,
  // $ "MinPunctCount":1,"MinLowerCount":1,"MinUpperCount":1,"MaxSpaceCount":0}'
  // - you can specify some JSON encoded parameters to change this default
  // values, which will validate the text field only if it contains from 5 to 10
  // characters, with at least one digit, one upper case letter, one lower case
  // letter, and one ponctuation sign, with no space allowed inside
  TSynValidatePassWord = class(TSynValidateText)
  protected
    /// set password specific parameters
    procedure SetParameters(Value: RawUTF8); override;
  end;

  /// will define a filter to be applied to a Record field content (typicaly
  // a TSQLRecord)
  // - a typical usage is to convert to lower or upper case, or
  // trim any time or date value in a TDateTime field
  // - the optional associated parameters are to be supplied JSON-encoded
  TSynFilter = class(TSynFilterOrValidate)
  protected
  public
    /// perform the filtering action to the specified value
    // - the value is converted into UTF-8 text, as expected by
    // TPropInfo.GetValue / TPropInfo.SetValue e.g.
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); virtual; abstract;
  end;

  TSynFilterClass = class of TSynFilter;

  /// a custom filter which will convert the value into Upper Case characters
  // - UpperCase conversion is made for ASCII-7 only, i.e. 'a'..'z' characters
  // - this version expects no parameter
  TSynFilterUpperCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// a custom filter which will convert the value into Upper Case characters
  // - UpperCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'e' acute will be converted to 'E'
  // - this version expects no parameter
  TSynFilterUpperCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// a custom filter which will convert the value into Lower Case characters
  // - LowerCase conversion is made for ASCII-7 only, i.e. 'A'..'Z' characters
  // - this version expects no parameter
  TSynFilterLowerCase = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// a custom filter which will convert the value into Lower Case characters
  // - LowerCase conversion is made for all latin characters in the WinAnsi
  // code page only, e.g. 'E' acute will be converted to 'e'
  // - this version expects no parameter
  TSynFilterLowerCaseU = class(TSynFilter)
  public
    /// perform the case conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;

  /// a custom filter which will trim any space character left or right to
  // the value
  // - this versions expect no parameter
  TSynFilterTrim = class(TSynFilter)
  public
    /// perform the space triming conversion to the specified value
    procedure Process(aFieldIndex: integer; var Value: RawUTF8); override;
  end;


{ ************ some other common types and conversion routines }

/// convert a string into its INTEGER Curr64 (value*10000) representation
// - this type is compatible with Delphi currency memory map with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - if NoDecimal is defined, will be set to TRUE if there is no decimal, AND
// the returned value will be an Int64 (not a PInt64(@Curr)^)
function StrToCurr64(P: PUTF8Char; NoDecimal: PBoolean=nil): Int64;

/// convert a string into its currency representation
// - will call StrToCurr64()
function StrToCurrency(P: PUTF8Char): currency;

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with Delphi currency memory map with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
function Curr64ToStr(Value: Int64): RawUTF8;

/// convert an INTEGER Curr64 (value*10000) into a string
// - this type is compatible with Delphi currency memory map with PInt64(@Curr)^
// - fast conversion, using only integer operations
// - decimals are joined by 2 (no decimal, 2 decimals, 4 decimals)
// - return the number of chars written to Dest^
function Curr64ToPChar(Value: Int64; Dest: PUTF8Char): PtrInt;

{/ internal fast INTEGER Curr64 (value*10000) value to text conversion
 - expect the last available temporary char position in P
 - return the last written char position (write in reverse order in P^)
 - will return 0 for Value=0, or a string representation with always 4 decimals
   (e.g. 1->'0.0001' 500->'0.0500' 25000->'2.5000' 30000->'3.0000')
 - is called by Curr64ToPChar() and Curr64ToStr() functions }
function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;

{/ truncate a Currency value to only 2 digits
  - implementation will use fast Int64 math to avoid any precision loss due to
    temporary floating-point conversion }
function RoundTo2Digits(Value: Currency): Currency;

var
  /// a conversion table from hexa chars into binary data
  // - returns 255 for any character out of 0..9,A..Z,a..z range
  // - used e.g. by HexToBin() function
  ConvertHexToBin: array[byte] of byte;

/// fast conversion from hexa chars into binary data
// - BinBytes contain the bytes count to be converted: Hex^ must contain
//  at least BinBytes*2 chars to be converted, and Bin^ enough space
// - if Bin=nil, no output data is written, but the Hex^ format is checked
// - return false if any invalid (non hexa) char is found in Hex^
// - using this function with Bin^ as an integer value will decode in big-endian
// order (most-signignifican byte first)
function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): boolean;

/// fast conversion from binary data into hexa chars
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with BinBytes^ as an integer value will encode it
// in low-endian order (less-signignifican byte first): don't use it for display
procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: integer); overload;

/// fast conversion from binary data into hexa chars
function BinToHex(const Bin: RawByteString): RawUTF8; overload;

/// fast conversion from binary data into hexa chars, ready to be displayed
// - BinBytes contain the bytes count to be converted: Hex^ must contain
// enough space for at least BinBytes*2 chars
// - using this function with Bin^ as an integer value will encode it
// in big-endian order (most-signignifican byte first): use it for display
procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: integer);

/// fast conversion from a pointer data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
function PointerToHex(aPointer: Pointer): RawUTF8;

/// fast conversion from a Cardinal data into hexa chars, ready to be displayed
// - use internally BinToHexDisplay()
function CardinalToHex(aCardinal: Cardinal): RawUTF8;

/// fast conversion from hexa chars into a pointer
function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: integer): boolean;

/// fast conversion from hexa chars into a cardinal
function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from binary data into Base64 encoded text
function BinToBase64(const s: RawByteString): RawByteString; overload;

/// fast conversion from binary data into Base64 encoded text
function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawByteString; overload;

/// fast conversion from binary data into Base64-like URI-compatible encoded text
// - will trim any right-sided '=' unsignificant characters, and replace
// '+' or '/' by '_' or '-'
function BinToBase64URI(Bin: PAnsiChar; BinBytes: integer): RawByteString;

/// fast conversion from binary data into Base64 encoded text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(const s: RawByteString): RawByteString; overload;

/// fast conversion from binary data into Base64 encoded text
// with JSON_BASE64_MAGIC prefix (UTF-8 encoded \uFFF0 special code)
function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawByteString; overload;

/// fast conversion from Base64 encoded text into binary data
function Base64ToBin(const s: RawByteString): RawByteString; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// fast conversion from Base64 encoded text into binary data
function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString; overload;

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(const s: RawByteString): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}

/// check if the supplied text is a valid Base64 encoded stream
function IsBase64(sp: PAnsiChar; len: PtrInt): boolean; overload;

/// retrieve the expected length of a Base64 encoded buffer
function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;

/// direct decoding of a Base64 encoded buffer
procedure Base64Decode(sp,rp: PAnsiChar; len: PtrInt);

/// add the 4 digits of integer Y to P^
procedure YearToPChar(Y: Word; P: PUTF8Char);
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// compare to floating point values, with IEEE 754 double precision
// - use this function instead of raw = operator
// - the precision is calculated from the A and B value range
// - faster equivalent than SameValue() in Math unit
// - if you know the precision range of A and B, it's faster to check abs(A-B)<range
function SameValue(const A, B: Double; DoublePrec: double = 1E-12): Boolean;

// our custom hash function, specialized for Text comparaison
// - has less colision than Adler32 for short strings
// - is faster than CRC32 or Adler32, since use DQWord (128 bytes) aligned read
// - uses RawByteString for binary content hashing, thatever the codepage is
function Hash32(const Text: RawByteString): cardinal; overload;
  {$ifdef HASINLINE}inline;{$endif}

// our custom hash function, specialized for Text comparaison
// - has less colision than Adler32 for short strings
// - is faster than CRC32 or Adler32, since use DQWord (128 bytes) aligned read
// - overloaded version for direct binary content hashing
function Hash32(Data: pointer; Len: integer): cardinal; overload;

/// standard Kernighan & Ritchie hash from "The C programming Language", 3rd edition
// - not the best, but simple and efficient code - perfect for THasher
function kr32(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;

var
  /// the default hasher used by TDynArrayHashed()
  // - is set to kr32() function above
  // - should be set to faster and more accurate crc32() function if available
  // (this is what SQLite3Commons unit does in its initialization block) 
  DefaultHasher: THasher;

/// retrieve a particular bit status from a bit array
function GetBit(const Bits; aIndex: PtrInt): boolean;
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// set a particular bit into a bit array
procedure SetBit(var Bits; aIndex: PtrInt);
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// unset/clear a particular bit into a bit array
procedure UnSetBit(var Bits; aIndex: PtrInt);
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// compute the number of bits set in a bit array
// - Count is the bit count, not byte size
function GetBitsCount(const Bits; Count: PtrInt): integer;

/// retrieve a particular bit status from a Int64 bit array (max aIndex is 63)
function GetBit64(const Bits; aIndex: PtrInt): boolean;
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// set a particular bit into a Int64 bit array (max aIndex is 63)
procedure SetBit64(var Bits: Int64; aIndex: PtrInt);
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// unset/clear a particular bit into a Int64 bit array (max aIndex is 63)
procedure UnSetBit64(var Bits: Int64; aIndex: PtrInt);
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// returns TRUE if all bytes equal zero
function IsZero(P: pointer; Length: integer): boolean; overload;

/// returns TRUE if no bit inside this TSQLFieldBits is set
// - is optimized for 64, 128, 192 and 256 max bits count (i.e. MAX_SQLFIELDS)
// - will work also with any other value 
function IsZero(const Fields: TSQLFieldBits): boolean; overload;
  {$ifdef HASINLINE}inline;{$endif}


{ ************ fast ISO-8601 types and conversion routines }

/// Date/Time conversion from ISO-8601
// - handle 'YYYYMMDDThhmmss' and 'YYYY-MM-DD hh:mm:ss' format
function Iso8601ToDateTime(const S: RawUTF8): TDateTime;
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// Date/Time conversion from ISO-8601
// - handle 'YYYYMMDDThhmmss' and 'YYYY-MM-DD hh:mm:ss' format
// - if L is left to default 0, it will be computed from StrLen(P)
function Iso8601ToDateTimePUTF8Char(P: PUTF8Char; L: integer=0): TDateTime;
  {$ifdef HASINLINE}inline;{$endif}

/// Date/Time conversion from ISO-8601
// - handle 'YYYYMMDDThhmmss' and 'YYYY-MM-DD hh:mm:ss' format
// - if L is left to default 0, it will be computed from StrLen(P)
procedure Iso8601ToDateTimePUTF8CharVar(P: PUTF8Char; L: integer; var result: TDateTime);

/// basic Date/Time conversion into ISO-8601
// - use 'YYYYMMDDThhmmss' format if not Expanded
// - use 'YYYY-MM-DDThh:mm:ss' format if Expanded
function DateTimeToIso8601(D: TDateTime; Expanded: boolean;
  FirstChar: AnsiChar='T'): RawUTF8;

/// basic Date conversion into ISO-8601
// - use 'YYYYMMDD' format if not Expanded
// - use 'YYYY-MM-DD' format if Expanded
function DateToIso8601(Date: TDateTime; Expanded: boolean): RawUTF8; overload;

/// basic Date conversion into ISO-8601
// - use 'YYYYMMDD' format if not Expanded
// - use 'YYYY-MM-DD' format if Expanded
function DateToIso8601(Y,M,D: cardinal; Expanded: boolean): RawUTF8; overload;

/// basic Time conversion into ISO-8601
// - use 'Thhmmss' format if not Expanded
// - use 'Thh:mm:ss' format if Expanded
function TimeToIso8601(Time: TDateTime; Expanded: boolean; FirstChar: AnsiChar='T'): RawUTF8;

/// Write a Date to P^ Ansi buffer
// - if Expanded is false, 'YYYYMMDD' date format is used
// - if Expanded is true, 'YYYY-MM-DD' date format is used
procedure DateToIso8601PChar(P: PUTF8Char; Expanded: boolean; Y,M,D: cardinal); overload;

/// Write a Date to P^ Ansi buffer
// - if Expanded is false, 'YYYYMMDD' date format is used
// - if Expanded is true, 'YYYY-MM-DD' date format is used
procedure DateToIso8601PChar(Date: TDateTime; P: PUTF8Char; Expanded: boolean); overload;

/// convert a date into 'YYYY-MM-DD' date format
// - resulting text is compatible with all ISO-8601 functions
function DateToIso8601Text(Date: TDateTime): RawUTF8;

/// write a TDateTime into strict ISO-8601 date and/or time text
// - if DT=0, returns ''
// - if DT contains only a date, returns the date encoded as 'YYYY-MM-DD'
// - if DT contains only a time, returns the time encoded as 'Thh:mm:ss'
// - otherwize, returns the ISO-8601 date and time encoded as 'YYYY-MM-DDThh:mm:ss'
// - used e.g. by TPropInfo.GetValue() and TPropInfo.NormalizeValue() methods
function DateTimeToIso8601Text(DT: TDateTime; FirstChar: AnsiChar='T'): RawUTF8;

/// Write a Time to P^ Ansi buffer
// - if Expanded is false, 'Thhmmss' time format is used
// - if Expanded is true, 'Thh:mm:ss' time format is used
// - you can custom the first char in from of the resulting text time
procedure TimeToIso8601PChar(P: PUTF8Char; Expanded: boolean; H,M,S: cardinal;
  FirstChar: AnsiChar = 'T'); overload;

/// Write a Time to P^ Ansi buffer
// - if Expanded is false, 'Thhmmss' time format is used
// - if Expanded is true, 'Thh:mm:ss' time format is used
// - you can custom the first char in from of the resulting text time
procedure TimeToIso8601PChar(Time: TDateTime; P: PUTF8Char; Expanded: boolean;
  FirstChar: AnsiChar = 'T'); overload;

/// convert a Iso8601 encoded string into a "fake" second count
// - use internally for computation an abstract "year" of 16 months of 32 days
// of 32 hours of 64 minutes of 64 seconds
// - use this function only for fast comparaison between two Iso8601 date/time
// - conversion is faster than Iso8601ToDateTime: use only binary integer math
// - ContainsNoTime optional pointer can be set to a boolean, which will be
// set according to the layout in P (e.g. TRUE for '2012-05-26')
// - returns 0 in case of invalid input string
function Iso8601ToSecondsPUTF8Char(P: PUTF8Char; L: integer; ContainsNoTime: PBoolean=nil): QWord;

/// convert a Iso8601 encoded string into a "fake" second count
// - use internally for computation an abstract "year" of 16 months of 32 days
// of 32 hours of 64 minutes of 64 seconds
// - use this function only for fast comparaison between two Iso8601 date/time
// - conversion is faster than Iso8601ToDateTime: use only binary integer math
function Iso8601ToSeconds(const S: RawUTF8): Int64;
  {$ifdef PUREPASCAL} {$ifdef HASINLINE}inline;{$endif} {$endif}

/// test if P^ contains a valid ISO-8601 text encoded value
// - calls internally Iso8601ToSecondsPUTF8Char() and returns true if contains
// at least a valid year (YYYY)
function IsIso8601(P: PUTF8Char; L: integer): boolean;
 {$ifdef HASINLINE}inline;{$endif}


type
  /// fast integer-encoded date and time value
  // - faster than Iso-8601 text and TDateTime
  // - e.g. can be used as published property field in TSQLRecord
  // - convenient for current date and time process (logging e.g.)
  // - use internally for computation an abstract "year" of 16 months of 32 days
  // of 32 hours of 64 minutes of 64 seconds - same as Iso8601ToSeconds()
  // - type cast any value of TTimeLog with the Iso8601 object below for easy
  // access to its content
  // - since TTimeLog type is bit-oriented, you can't just use add or substract
  // two TTimeLog values when doing such date/time computation: use a TDateTime
  // temporary conversion in such case
  TTimeLog = type Int64;

  /// dynamic array of TTimeLog
  // - used by TDynArray JSON serialization to handle textual serialization
  TTimeLogDynArray = array of TTimeLog;

  /// pointer to Iso8601
  PIso8601 = ^Iso8601;

  /// usefull object to type cast TTimeLog type into Iso-8601 or TDateTime
  // - typecast TTimeLog from PIso8601, not with Iso8601(Time).From():
  // PIso8601(@aTime)^.From()
  // - to get current time, simply use Time := Iso8601Now
  // - since Iso8601.Value is bit-oriented, you can't just use add or substract
  // two TTimeLog values when doing such date/time computation: use a TDateTime
  // temporary conversion in such case
  Iso8601 = {$ifndef UNICODE}object{$else}record{$endif}
    /// the value itself
    // - bits 0..5   = Seconds (0..59)
    // - bits 6..11  = Minutes (0..59)
    // - bits 12..16 = Hours   (0..23)
    // - bits 17..21 = Day-1   (0..31)
    // - bits 22..25 = Month-1 (0..11)
    // - bits 26..38 = Year    (0..4095)
    Value: Int64;
{$ifdef MSWINDOWS}
    /// extract the date and time content in Value into individual values
    procedure Expand(out Date: TSystemTime);
{$endif}
    /// convert to Iso-8601 encoded text
    function Text(Expanded: boolean; FirstTimeChar: AnsiChar = 'T'): RawUTF8; overload;
    /// convert to Iso-8601 encoded text
    function Text(Dest: PUTF8Char; Expanded: boolean;
      FirstTimeChar: AnsiChar = 'T'): integer; overload;
    /// convert to a delphi Time
    function ToTime: TDateTime;
    /// convert to a delphi Date
    function ToDate: TDateTime;
    /// convert to a delphi Date and Time
    function ToDateTime: TDateTime;
    /// fill Value from specified Date and Time
    procedure From(Y,M,D, HH,MM,SS: cardinal); overload;
    /// fill Value from specified TDateTime
    procedure From(DateTime: TDateTime; DateOnly: Boolean=false); overload;
    /// fill Value from specified File Date
    procedure From(FileDate: integer); overload;
    /// fill Value from Iso-8601 encoded text
    procedure From(P: PUTF8Char; L: integer); overload;
    /// fill Value from Iso-8601 encoded text
    procedure From(const S: RawUTF8); overload;
    /// fill Value from current local system Date and Time
    procedure FromNow;
  end;

  TIso8601Array = array[0..maxInt div sizeof(Int64)-1] of Iso8601;
  PIso8601Array = ^TIso8601Array;

/// get TTimeLog value from current date and time
function Iso8601Now: Int64;
  {$ifdef HASINLINE}inline;{$endif}

/// get TTimeLog value from a file date and time
function Iso8601FromFile(const FileName: TFileName): Int64;

/// get TTimeLog value from a given Delphi date and time
function Iso8601FromDateTime(DateTime: TDateTime): Int64;
  {$ifdef HASINLINE}inline;{$endif}

/// retrieve the current Date, in the ISO 8601 layout, but expanded and
// ready to be displayed
function NowToString(Expanded: boolean=true; FirstTimeChar: AnsiChar = ' '): RawUTF8;

/// retrieve the current Time (whithout Date), in the ISO 8601 layout
// - usefull for direct on screen logging e.g.
function TimeToString: RawUTF8;

var
  /// custom date to ready to be displayed text function
  // - you can override this pointer in order to display the text according
  // to your current i18n settings
  // - used by TSQLTable.ExpandAsString() method, i.e. TSQLTableToGrid.DrawCell()
  i18nDateText: function(Iso: TTimeLog): string = nil;


{$ifndef ENHANCEDRTL}
{$ifndef LVCL} { don't define these twice }

var
  /// these procedure type must be defined if a default system.pas is used
  // - SQLite3i18n unit will hack default LoadResString() procedure
  // - already defined in our Extended system.pas unit
  // - needed with FPC, Delphi 2009 and up, i.e. when ENHANCEDRTL is not defined
  // - expect generic "string" type, i.e. UnicodeString for Delphi 2009+
  // - not needed with the LVCL framework (we should be on server side)
  LoadResStringTranslate: procedure(var Text: string) = nil;

  /// current LoadResString() cached entries count
  // - i.e. resourcestring caching for faster use
  // - used only if a default system.pas is used, not our Extended version
  // - defined here, but resourcestring caching itself is implemented in the
  // SQLite3i18n.pas unit, if the ENHANCEDRTL conditional is not defined
  CacheResCount: integer = -1;

{$endif}
{$endif}


/// log a message to a local text file
// - the text file is located in the executable directory, and its name is
// simply the executable file name with the '.log' extension instead of '.exe'
// - format contains the current date and time, then the Msg on one line
// - date and time format used is 'YYYYMMDD hh:mm:ss (i.e. ISO-8601)'
procedure LogToTextFile(Msg: RawUTF8);

/// log a message to a local text file
// - this version expect the filename to be specified
// - format contains the current date and time, then the Msg on one line
// - date and time format used is 'YYYYMMDD hh:mm:ss'
procedure AppendToTextFile(aLine: RawUTF8; const aFileName: TFileName);

const
  /// Rotate local log file if reached this size (1MB by default)
  // - .log file will be save as .log.bak file
  // - a new .log file is created
  MAXLOGSIZE = 1024*1024;


{ ************ fast low-level lookup types used by internal conversion routines }

{$ifndef ENHANCEDRTL}
{$ifndef LVCL} { don't define these const twice }

const
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  // - our enhanced SysUtils.pas (normal and LVCL) contains the same array
  TwoDigitLookup: packed array[0..99] of array[1..2] of AnsiChar =
    ('00','01','02','03','04','05','06','07','08','09',
     '10','11','12','13','14','15','16','17','18','19',
     '20','21','22','23','24','25','26','27','28','29',
     '30','31','32','33','34','35','36','37','38','39',
     '40','41','42','43','44','45','46','47','48','49',
     '50','51','52','53','54','55','56','57','58','59',
     '60','61','62','63','64','65','66','67','68','69',
     '70','71','72','73','74','75','76','77','78','79',
     '80','81','82','83','84','85','86','87','88','89',
     '90','91','92','93','94','95','96','97','98','99');

{$endif}
{$endif}

var
  /// fast lookup table for converting any decimal number from
  // 0 to 99 into their ASCII equivalence
  TwoDigitLookupW: packed array[0..99] of word absolute TwoDigitLookup;

const
  /// used internaly for fast word recognition (32 bytes const)
  IsWord: set of byte =
    [ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')];

  /// used internaly for fast identifier recognition (32 bytes const)
  // - can be used e.g. for field or table name
  // - this char set matches the classical pascal definition of identifiers
  IsIdentifier: set of byte =
    [ord('_'),ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z')];


{$ifdef MSWINDOWS}

{$M+} // to have existing RTTI for published properties
type
  /// used to retrieve version information from any EXE
  TFileVersion = class
  protected
    fDetailed: string;
    fBuildDateTime: TDateTime;
  public
    /// executable major version number
    Major: Integer;
    /// executable minor version number
    Minor: Integer;
    /// executable release version number
    Release: Integer;
    /// executable release build number
    Build: Integer;
    /// build year of this exe file
    BuildYear: integer;
    /// version info of the exe file as '3.1'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    Main: string;
    /// retrieve application version from exe file name
    // - DefaultVersion is used if no information Version was included into
    // the executable resources (on compilation time)
    // - to retrieve version information from current executable, just call
    // ExeVersionRetrieve function, then use ExeVersion global variable
    constructor Create(const FileName: TFileName; DefaultVersion: integer);
    /// retrieve the version as a 32 bits integer with Major.Minor.Release
    function Version32: integer;
  published
    /// version info of the exe file as '3.1.0.123'
    // - return "string" type, i.e. UnicodeString for Delphi 2009+
    property Detailed: string read fDetailed write fDetailed;
    /// build date and time of this exe file
    property BuildDateTime: TDateTime read fBuildDateTime write fBuildDateTime;
  end;
{$M-}


{$ifdef DELPHI6OROLDER}
{/ GetFileVersion returns the most significant 32 bits of a file's binary
  version number
  - Typically, this includes the major and minor version placed
  together in one 32-bit integer
  - It generally does not include the release or build numbers
  - It returns Cardinal(-1) if it failed }
function GetFileVersion(const FileName: TFileName): cardinal;
{$endif}

type
  /// the recognized Windows versions
  TWindowsVersion = (
    wUnknown, w2000, wXP, wXP_64, wServer2003, wServer2003_R2,
    wVista, wVista_64, wServer2008, wServer2008_64,
    wServer2008_R2, wServer2008_R2_64, wSeven, wSeven_64,
    wEight, wEight_64, wEightServer, wEightServer_64);
  {$ifndef UNICODE}
  /// not defined in older Delphi versions
  TOSVersionInfoEx = record
    dwOSVersionInfoSize: DWORD;
    dwMajorVersion: DWORD;
    dwMinorVersion: DWORD;
    dwBuildNumber: DWORD;
    dwPlatformId: DWORD;
    szCSDVersion: array[0..127] of char;
    wServicePackMajor: WORD;
    wServicePackMinor: WORD;
    wSuiteMask: WORD;
    wProductType: BYTE;
    wReserved:BYTE;
  end;
  {$endif}


var
  /// is set to TRUE if the current process is running under WOW64
  // - WOW64 is the x86 emulator that allows 32-bit Windows-based applications
  // to run seamlessly on 64-bit Windows
  IsWow64: boolean;
  /// the current System information, as retrieved for the current process
  // - under a WOW64 process, it will use the GetNativeSystemInfo() new API
  // to retrieve the real top-most system information
  // - note that the lpMinimumApplicationAddress field is replaced by a
  // more optimistic/realistic value ($100000 instead of default $10000)
  SystemInfo: TSystemInfo;
  /// the current Operating System information, as retrieved for the current process
  OSVersionInfo: TOSVersionInfoEx;
  /// the current Operating System version, as retrieved for the current process
  OSVersion: TWindowsVersion;

  /// global information about the current executable and computer
  // - call ExeVersionRetrieve before using it
  ExeVersion: record
    /// the main executable name, without any path nor extension
    ProgramName: RawUTF8;
    /// the main executable details, as used e.g. by TSynLog
    // - e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-03-29 11:09:06)'
    ProgramFullSpec: RawUTF8;
    /// the main executable file name (including full path)
    // - same as paramstr(0)
    ProgramFileName: TFileName;
    /// the main executable full path (excluding .exe file name) 
    // - same as ExtractFilePath(paramstr(0))
    ProgramFilePath: TFileName;
    /// the full path of the running executable or library
    // - for an executable, same as paramstr(0) 
    // - for a library, will contain the whole .dll file name 
    InstanceFileName: TFileName;
    /// the current executable version
    Version: TFileVersion;
    /// the current computer host name
    Host: RawUTF8;
    /// the current computer user name
    User: RawUTF8;
  end;

/// initialize ExeVersion global variable, if not already done
procedure ExeVersionRetrieve(DefaultVersion: integer=0);

{/ this function can be used to create a GDI compatible window, able to
  receive GDI messages for fast local communication
  - will return 0 on failure (window name already existing e.g.), or
    the created HWND handle on success
  - it will call the supplied message handler defined for a given GDI message:
    for instance, define such a method in any object definition:
  !  procedure WMCopyData(var Msg : TWMCopyData); message WM_COPYDATA; }
function CreateInternalWindow(const aWindowName: string; aObject: TObject): HWND;

{/ delete the window resources used to receive GDI messages
  - must be called for each CreateInternalWindow() function
  - both parameter values are then reset to ''/0 }
function ReleaseInternalWindow(var aWindowName: string; var aWindow: HWND): boolean;

type
  TPatchCode = array[0..4] of byte;
  PPatchCode = ^TPatchCode;
  
/// Self-modifying code - change some memory buffer in the code segment
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overriden code buffer, for further hook disabling
procedure PatchCode(Old,New: pointer; Size: integer; Backup: pointer=nil);

/// Self-modifying code - change one PtrUInt in the code segment
procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt);

/// Self-modifying code - add an asm JUMP to a redirected function
// - if Backup is not nil, it should point to a Size array of bytes, ready
// to contain the overriden code buffer, for further hook disabling
procedure RedirectCode(Func, RedirectFunc: Pointer; Backup: PPatchCode=nil);

/// Self-modifying code - restore a code from its RedirectCode() backup
procedure RedirectCodeRestore(Func: pointer; const Backup: TPatchCode);
{$else}

/// compatibility function, to be implemented according to the running OS
// - expect more or less the same result as the homonymous Win32 API function
function GetTickCount: Cardinal;

{$endif MSWINDOWS}

type
  /// to be used instead of TMemoryStream, for speed
  // - allocates memory from Delphi heap (i.e. FastMM4/SynScaleMM)
  // and not GlobalAlloc()
  // - uses bigger growing size of the capacity
{$ifdef LVCL} // LVCL already use Delphi heap instead of GlobalAlloc()
  THeapMemoryStream = TMemoryStream;
{$else}
  THeapMemoryStream = class(TMemoryStream)
  protected
    function Realloc(var NewCapacity: Longint): Pointer; override;
  end;
{$endif}

var
  /// a global "Garbage collector", for some classes instances which must
  // live for all the main executable process
  // - used to avoid any memory leak with e.g. 'class var RecordProps'
  GarbageCollector: TObjectList;


{ ************ TSynTable generic types and classes }

{$define SORTCOMPAREMETHOD}
{ if defined, the field content comparison will use a method instead of fixed
  functions - could be mandatory for tftArray field kind }

type




  {$ifdef SORTCOMPAREMETHOD}
  /// internal value used by TSynTableFieldProperties.SortCompare() method to
  // avoid stack allocation
  TSortCompareTmp = record
    PB1, PB2: PByte;
    L1,L2: integer;
  end;
  {$endif}

  



const
  /// JSON compatible representation of a boolean value
  JSON_BOOLEAN: array[boolean] of RawUTF8 = ('false','true');

  /// used by TSynTableStatement.WhereField for "SELECT .. FROM TableName WHERE ID=?"
  SYNTABLESTATEMENTWHEREID = 0;
  /// used by TSynTableStatement.WhereField for "SELECT * FROM TableName"
  SYNTABLESTATEMENTWHEREALL = -1;
  /// used by TSynTableStatement.WhereField for "SELECT Count(*) FROM TableName"
  SYNTABLESTATEMENTWHERECOUNT = -2;






/// convert a size to a human readable value
// - append MB, KB or B symbol
// - for MB and KB, add one fractional digit
function KB(bytes: Int64): RawUTF8;

/// convert a micro seconds elapsed time into a human readable value
// - append us, ms or s symbol
// - for us and ms, add two fractional digits
function MicroSecToString(Micro: Int64): RawUTF8;

/// convert an integer value into its textual representation with thousands marked
// - ThousandSep is the character used to separate thousands in numbers with
// more than three digits to the left of the decimal separator
function IntToThousandString(Value: integer; const ThousandSep: RawUTF8=','): RawUTF8;



{ ************ Logging classes and functions }

{$ifdef CPU64}
  // defines if exceptions shall not be intercepted
  {$define NOEXCEPTIONINTERCEPT}
{$endif}

{$ifdef FPC}
  // exceptions interception code in FPC differs from Delphi
  {$define NOEXCEPTIONINTERCEPT}
{$endif}

type
  /// a debugger symbol, as decoded by TSynMapFile from a .map file
  TSynMapSymbol = record
    /// symbol internal name
    Name: RawUTF8;
    /// starting offset of this symbol in the executable
    Start: cardinal;
    /// end offset of this symbol in the executable
    Stop: cardinal;
  end;
  PSynMapSymbol = ^TSynMapSymbol;
  /// a dynamic array of symbols, as decoded by TSynMapFile from a .map file
  TSynMapSymbolDynArray = array of TSynMapSymbol;

  /// a debugger unit, as decoded by TSynMapFile from a .map file
  TSynMapUnit = record
    /// Name, Start and Stop of this Unit
    Symbol: TSynMapSymbol;
    /// associated source file name
    FileName: RawUTF8;
    /// list of all mapped source code lines of this unit
    Line: TIntegerDynArray;
    /// start code address of each source code lin
    Addr: TIntegerDynArray;
  end;
  /// a dynamic array of units, as decoded by TSynMapFile from a .map file
  TSynMapUnitDynArray = array of TSynMapUnit;

  {$M+}
  /// retrieve a .map file content, to be used e.g. with TSynLog to provide
  // additional debugging information
  // - original .map content can be saved as .mab file in a more optimized format
  TSynMapFile = class
  protected
    fMapFile: TFileName;
    fSymbol: TSynMapSymbolDynArray;
    fUnit: TSynMapUnitDynArray;
    fSymbols: TDynArray;
    fUnits: TDynArrayHashed;
    fGetModuleHandle: PtrUInt;
    fHasDebugInfo: boolean;
  public
    /// get the available debugging information
    // - if aExeName is specified, will use it in its search for .map/.mab
    // - if aExeName is not specified, will use the currently running .exe/.dll
    // - it will first search for a .map matching the file name: if found,
    // will be read to retrieve all necessary debugging information - a .mab
    // file will be also created in the same directory (if MabCreate is TRUE)
    // - if .map is not not available, will search for the .mab file
    // - if no .mab is available, will search for a .mab appended to the .exe/.dll
    // - if nothing is available, will log as hexadecimal pointers, without
    // debugging information
    constructor Create(const aExeName: TFileName=''; MabCreate: boolean=true);
    /// save all debugging information in the .mab custom binary format
    // - if no file name is specified, it will be saved as ExeName.mab or DllName.mab
    // - this file content can be appended to the executable via SaveToExe method
    // - this function returns the created file name
    function SaveToFile(const aFileName: TFileName=''): TFileName;
    /// save all debugging informat in our custom binary format
    procedure SaveToStream(aStream: TStream);
    /// append all debugging information to an executable (or library)
    // - the executable name must be specified, because it's impossible to
    // write to the executable of a running process
    // - this method will work for .exe and for .dll (or .ocx)
    procedure SaveToExe(const aExeName: TFileName);
    /// add some debugging information according to the specified memory address
    // - will create a global TSynMapFile instance for the current process, if
    // necessary
    // - if no debugging information is available (.map or .mab), will write
    // the address as hexadecimal
    class procedure Log(W: TTextWriter; Addr: PtrUInt);
    /// retrieve a symbol according to an absolute code address
    function FindSymbol(aAddr: cardinal): integer;
    /// retrieve an unit and source line, according to an absolute code address
    function FindUnit(aAddr: cardinal; out LineNumber: integer): integer;
    /// return the symbol location according to the supplied absolute address
    // - i.e. unit name, symbol name and line number (if any), as plain text
    // - returns '' if no match found
    function FindLocation(aAddr: Cardinal): RawUTF8;
    /// all symbols associated to the executable
    property Symbols: TSynMapSymbolDynArray read fSymbol;
    /// all units, including line numbers, associated to the executable
    property Units: TSynMapUnitDynArray read fUnit;
  published
    /// the associated file name
    property FileName: TFileName read fMapFile;
    /// equals true if a .map or .mab debugging information has been loaded
    property HasDebugInfo: boolean read fHasDebugInfo;
  end;
  {$M-}
  
  /// the available logging events, as handled by TSynLog
  // - sllInfo will log general information events
  // - sllDebug will log detailed debugging information
  // - sllTrace will log low-level step by step debugging information
  // - sllWarning will log unexpected values (not an error)
  // - sllError will log errors
  // - sllEnter will log every method start
  // - sllLeave will log every method quit
  // - sllLastError will log the GetLastError OS message
  // - sllException will log all exception raised - available since Windows XP
  // - sllExceptionOS will log all OS low-level exceptions (EDivByZero,
  // ERangeError, EAccessViolation...)
  // - sllMemory will log memory statistics
  // - sllStackTrace will log caller's stack trace (it's by default part of
  // TSynLogFamily.LevelStackTrace like sllError, sllException, sllExceptionOS,
  // sllLastError and sllFail)
  // - sllFail was defined for TSynTestsLogged.Failed method, and can be used
  // to log some customer-side assertions (may be notifications, not errors)
  // - sllSQL is dedicated to trace the SQL statements
  // - sllCache should be used to trace the internal caching mechanism
  // - sllResult could trace the SQL results, JSON encoded
  // - sllDB is dedicated to trace low-level database engine features
  // - sllHTTP could be used to trace HTTP process
  // - sllClient/sllServer could be used to trace some Client or Server process
  // - sllServiceCall/sllServiceReturn to trace some remote service or library
  // - sllUserAuth to trace user authentication (e.g. for individual requests)
  // - sllCustom* items can be used for any purpose
  TSynLogInfo = (
    sllNone, sllInfo, sllEvent, sllDebug, sllPortWrite, sllPortRead, sllTrace, sllWarning, sllError,
    sllEnter, sllLeave,
    sllLastError, sllException, sllExceptionOS, sllMemory, sllStackTrace,
    sllFail, sllSQL, sllCache, sllResult, sllDB, sllHTTP, sllClient, sllServer,
    sllServiceCall, sllServiceReturn, sllUserAuth, sllCustom2);

  /// used to define a logging level
  // - i.e. a combination of none or several logging event
  // - e.g. use LOG_VERBOSE constant to log all events
  TSynLogInfos = set of TSynLogInfo;

  /// a dynamic array of logging event levels
  TSynLogInfoDynArray = array of TSynLogInfo;

  {$M+} { we need the RTTI for the published methods of the logging classes }

  TSynLog = class;
  //TSynLogClass = class of TSynLog;
  TSynLogFamily = class;
  TSynLogFile = class;
  
  {$M-}

  /// a generic interface used for logging a method
  // - you should create one TSynLog instance at the beginning of a block code
  // using TSynLog.Enter: the ISynLog will be released automaticaly by the
  // compiler at the end of the method block, marking it's executation end
  // - all logging expect UTF-8 encoded text, i.e. usualy English text
(*  ISynLog = interface(IUnknown)
    ['{527AC81F-BC41-4717-B089-3F74DE56F1AE}']
{$ifndef DELPHI5OROLDER}
    /// call this method to add some information to the log at a specified level
    // - see the format in TSynLog.Log() method description
    // (not compatible with default SysUtils.Format function)
    // - if Instance is set, it will log the corresponding class name and address
    // (to be used if you didn't call TSynLog.Enter() method first)
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArgs: array of const;
      Instance: TObject=nil); overload;
{$endif}
    /// call this method to add some information to the log at a specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first)
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject=nil); overload;
    /// call this method to add the content of an object to the log at a
    // specified level
    // - TSynLog will write the class and hexa address - TSQLLog will write the
    // object JSON content
    procedure Log(Level: TSynLogInfo; Instance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - TSynLog will handle enumerations and dynamic array; TSQLLog will be
    // able to write TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; aName: PWinAnsiChar;
      aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo=sllTrace); overload;
    /// retrieve the associated logging instance
    function Instance: TSynLog;
  end;   *)

  /// this event can be set for TSynLogFamily to archive any deprecated log
  // into a custom compressed format
  // - will be called by TSynLogFamily when TSynLogFamily.Destroy identify
  // some outdated files
  // - the aOldLogFileName will contain the .log file with full path
  // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
  // - should return true on success, false on error
  // - example of matching event handler are EventArchiveDelete/EventArchiveSynLZ
  // or EventArchiveZip in SynZip.pas
  // - this event handler will be called one time per .log file to archive,
  // then one last time with aOldLogFileName='' in order to close any pending
  // archive (used e.g. by EventArchiveZip to open the .zip only once)
  TSynLogArchiveEvent = function(const aOldLogFileName, aDestinationPath: TFileName): boolean of object;

  {/ regroup several logs under an unique family name
   - you should usualy use one family per application or per architectural
     module: e.g. a server application may want to log in separate files the
     low-level Communication, the DB access, and the high-level process
   - initialize the family settings before using them, like in this code:
     ! with TSynLogDB.Family do begin
     !   Level := LOG_VERBOSE;
     !   PerThreadLog := true;
     !   DestinationPath := 'C:\Logs';
     ! end;
   - then use the logging system inside a method:
     ! procedure TMyDB.MyMethod;
     ! var ILog: ISynLog;
     ! begin
     !   ILog := TSynLogDB.Enter(self,'MyMethod');
     !   // do some stuff
     !   ILog.Log(sllInfo,'method called');
     ! end; }
  TSynLogFamily = class
  private
    fArchiveAfterDays: Integer;
    fArchivePath: TFileName;
    fOnArchive: TSynLogArchiveEvent;
    fBeforeDestroy : TNotifyEvent;
  protected
    fLevel, fLevelStackTrace: TSynLogInfos;
   // fPerThreadLog: boolean;
//    fIncludeComputerNameInFileName: boolean;
 //   fGlobalLog: TSynLog;
    //fSynLogClass: TSynLogClass;
    fIdent: integer;
    fDestinationPath: TFileName;
    fDefaultExtension: TFileName;
    fBufferSize: integer;
    fHRTimeStamp: boolean;
    fWithUnitName: boolean;
    fMaxMBSize :Word;
    fAutoFlush: cardinal;
    {$ifndef NOEXCEPTIONINTERCEPT}
    fHandleExceptions: boolean;
    {$endif}
    fStackTraceLevel: byte;
    fStackTraceUseOnlyAPI: boolean;
    fExceptionIgnore: TList;
    function CreateSynLog: TSynLog;
    procedure SetAutoFlush(TimeOut: cardinal);
    procedure SetLevel(aLevel: TSynLogInfos);
  public
    /// intialize for a TSynLog class family
    // - add it in the global SynLogFileFamily[] list
    constructor Create{(aSynLog: TSynLogClass)};
    /// release associated memory
    // - will archive older DestinationPath\*.log files, according to
    // ArchiveAfterDays value and ArchivePath
    destructor Destroy; override;
    /// retrieve the corresponding log file of this thread and family
    // - creates the TSynLog if not already existing for this current thread
    function SynLog: TSynLog;
    /// you can add some exceptions to be ignored to this list
    // - for instance, EConvertError may be added to the list
    property ExceptionIgnore: TList read fExceptionIgnore;
    /// event called to archive the .log content after a defined delay
    // - Destroy will parse DestinationPath folder for *.log files matching
    // ArchiveAfterDays property value
    // - you can set this property to EventArchiveDelete in order to delete deprecated
    // files, or EventArchiveSynLZ to compress the .log file into our propertary
    // SynLZ format: resulting file name will be ArchivePath\log\YYYYMM\*.log.synlz
    // (use FileUnSynLZ function to uncompress it)
    // - if you use SynZip.EventArchiveZip, the log files will be archived in
    // ArchivePath\log\YYYYMM.zip
    // - the aDestinationPath parameter will contain 'ArchivePath\log\YYYYMM\'
    // - this event handler will be called one time per .log file to archive,
    // then one last time with aOldLogFileName='' in order to close any pending
    // archive (used e.g. by EventArchiveZip to open the .zip only once)
    property OnArchive: TSynLogArchiveEvent read fOnArchive write fOnArchive;

    property BeforeDestroy : TNotifyEvent read fBeforeDestroy write fBeforeDestroy;
  published
    /// the associated TSynLog class
   // property SynLogClass: TSynLogClass read fSynLogClass;
    /// index in global SynLogFileFamily[] and threadvar SynLogFileIndex[] lists
    property Ident: integer read fIdent;
    /// the current level of logging information for this family
    // - can be set e.g. to LOG_VERBOSE in order to log every kind of events
    property Level: TSynLogInfos read fLevel write SetLevel;
    /// the levels which will include a stack trace of the caller
    // - by default, contains sllError, sllException, sllExceptionOS, sllFail,
    // sllLastError and sllStackTrace
    // - exceptions will always trace the stack
    property LevelStackTrace: TSynLogInfos read fLevelStackTrace write fLevelStackTrace;
    /// the folder where the log must be stored
    // - by default, is in the executable folder
    property DestinationPath: TFileName read fDestinationPath write fDestinationPath;
    /// the file extension to be used
    // - is '.log' by default
    property DefaultExtension: TFileName read fDefaultExtension write fDefaultExtension;
    /// the folder where old log files must be compressed
    // - by default, is in the executable folder, i.e. the same as DestinationPath
    // - the 'log\' sub folder name will always be appended to this value
    // - will then be used by OnArchive event handler to produce, with the
    // current file date year and month, the final path (e.g.
    // 'ArchivePath\Log\YYYYMM\*.log.synlz' or 'ArchivePath\Log\YYYYMM.zip')
    property ArchivePath: TFileName read fArchivePath write fArchivePath;
    /// number of days before OnArchive event will be called to compress
    // or delete deprecated files
    // - will be set by default to 7 days
    // - will be used by Destroy to call OnArchive event handler on time
    property ArchiveAfterDays: Integer read fArchiveAfterDays write fArchiveAfterDays;
    /// the internal in-memory buffer size, in bytes
    // - this is the number of bytes kept in memory before flushing to the hard
    // drive; you can call TSynLog.Flush method or set AutoFlushTimeOut to true
    // in order to force the writting to disk
    // - is set to 4096 by default (4 KB is the standard hard drive cluster size)
    property BufferSize: integer read fBufferSize write fBufferSize;
    /// if TRUE, each thread will have its own logging file
   // property PerThreadLog: boolean read fPerThreadLog write fPerThreadLog;
    /// if TRUE, the log file name will contain the Computer name - as '(MyComputer)'
  //  property IncludeComputerNameInFileName: boolean read fIncludeComputerNameInFileName write fIncludeComputerNameInFileName;
    /// if TRUE, will log high-resolution time stamp instead of ISO 8601 date and time
    // - this is less human readable, but allows performance profiling of your
    // application on the customer side (using TSynLog.Enter methods)
    property HighResolutionTimeStamp: boolean read fHRTimeStamp write fHRTimeStamp;
    /// if TRUE, will log the unit name with an object instance if available
    // - unit name is available from RTTI if the class has published properties
    property WithUnitName: boolean read fWithUnitName write fWithUnitName;
    //File size Limit on Megabytes
    // If limit is over, the log file will be recreated
    property MaxMBSize : Word read fMaxMBSize write fMaxMBSize;
    /// the time (in seconds) after which the log content must be written on
    // disk, whatever the current content size is
    // - by default, the log file will be written for every 4 KB of log - this
    // will ensure that the main application won't be slow down by logging
    // - in order not to loose any log, a background thread can be created
    // and will be responsible of flushing all pending log content every
    // period of time (e.g. every 10 seconds)
    property AutoFlushTimeOut: cardinal read fAutoFlush write SetAutoFlush;
    /// the recursive depth of stack trace symbol to write
    // - used only if exceptions are handled, or by sllStackTrace level
    // - default value is 20, maximum is 255
    property StackTraceLevel: byte read fStackTraceLevel write fStackTraceLevel;
    /// if the stack trace shall use only the Windows API
    // - the class will use low-level RtlCaptureStackBackTrace() API to retrieve
    // the call stack: in some cases, it is not able to retrieve it, therefore
    // a manual walk of the stack can be processed - since this manual call can
    // trigger some unexpected access violations or return wrong positions,
    // you can disable this optional manual walk by setting this property to TRUE
    // - default is FALSE, i.e. use RtlCaptureStackBackTrace() API and perform
    // a manual stack walk if the API returned to address
    property StackTraceUseOnlyAPI: boolean read fStackTraceUseOnlyAPI write fStackTraceUseOnlyAPI;
  end;

  /// used by ISynLog/TSynLog.Enter methods to handle recursivity calls tracing
  TSynLogCurrentIdent = packed record
    /// associated class instance to be displayed
    Instance: TObject;
    /// associated class type to be displayed
    ClassType: TClass;
    /// the method name (or message) to be displayed
    Method: PUTF8Char;
    /// the caller address, ready to display stack trace dump if needed
    Caller: PtrUInt;
    /// the time stamp at enter time
    EnterTimeStamp: Int64;
  end;
  PSynLogCurrentIdent = ^TSynLogCurrentIdent;
  /// used to store the identification of all recursivity levels
  TSynLogCurrentIdents = array[0..maxInt div sizeof(TSynLogCurrentIdent)-1] of TSynLogCurrentIdent;
  PSynLogCurrentIdents = ^TSynLogCurrentIdents;

  /// a per-family and/or per-thread log file content
  // - you should create a sub class per kind of log file
  // ! TSynLogDB = class(TSynLog);
  // - the TSynLog instance won't be allocated in heap, but will share a
  // per-thread (if Family.PerThreadLog=TRUE) or global private log file instance
  // - was very optimized for speed, if no logging is written, and even during
  // log write (using an internal TTextWriter)
  // - can use available debugging information via the TSynMapFile class, for
  // stack trace logging for exceptions, sllStackTrace, and Enter/Leave labelling
  TSynLog = class(TObject{, ISynLog})
  protected
    fFamily: TSynLogFamily;
    fWriter: TTextWriter;
    fWriterStream: TStream;
    fHeaderWritten: boolean;
    fLockCS: PRTLCriticalSection;
    fStartTimeStamp: Int64;
    fCurrentTimeStamp: Int64;
    fFrequencyTimeStamp: Int64;
    fFileName: TFileName;
    /// set by Enter() method
    fRecursionCount: integer;
    fRecursionMax: integer;
    fRecursion: PSynLogCurrentIdents;
    fRecursionCaller: PtrUInt;
  (*  {$ifdef FPC}
    function QueryInterface({$IFDEF FPC_HAS_CONSTREF}constref{$ELSE}const{$ENDIF} iid : tguid;out obj) : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _AddRef : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    function _Release : longint;{$IFNDEF WINDOWS}cdecl{$ELSE}stdcall{$ENDIF};
    {$else}
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {$endif}    *)
  //  class function FamilyCreate: TSynLogFamily;
    procedure DoEnterLeave(aLevel: TSynLogInfo);
    procedure CreateLogWriter; virtual;
{$ifndef DELPHI5OROLDER}
    procedure LogInternal(Level: TSynLogInfo; TextFmt: PWinAnsiChar;
      const TextArgs: array of const; Instance: TObject); overload;
{$endif}
    procedure LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
      Instance: TObject); overload;
    procedure LogInternal(Level: TSynLogInfo; aName: PWinAnsiChar;
     aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload;
    // any call to this method MUST call UnLock
    procedure LogHeader(Level: TSynLogInfo);
    procedure LogTrailer(Level: TSynLogInfo); {$ifdef HASINLINE}inline;{$endif}
    procedure LogFileHeader; virtual;
{$ifndef DELPHI5OROLDER}
    procedure AddMemoryStats; virtual;
{$endif}
    procedure AddErrorMessage;
    procedure AddStackTrace(Stack: PPtrUInt);
    procedure CreateLocker;
    procedure Lock;   {$ifdef HASINLINE}inline;{$endif}
    procedure UnLock; {$ifdef HASINLINE}inline;{$endif}
    procedure RecursionGrow;
    procedure AddTyped(aTypeInfo: pointer; var aValue); virtual;
    procedure AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
   // function Instance: TSynLog;
  public
    /// intialize for a TSynLog class instance
    // - WARNING: not to be called directly! Use Enter or Add class function instead
    constructor Create(aFamily: TSynLogFamily=nil);
    /// release all memory and internal handles
    destructor Destroy; override;
    /// flush all log content to file
    // - if ForceDiskWrite is TRUE, will wait until written on disk (slow)
    procedure Flush(ForceDiskWrite: boolean);
    {/ overloaded method which will log the
     - this is the main method to be called within a method:
     ! procedure TMyDB.SQLExecute(const SQL: RawUTF8);
     ! var ILog: ISynLog;
     ! begin
     !   ILog := TSynLogDB.Enter(self,'SQLExecute');
     !   // do some stuff
     !   ILog.Log(sllInfo,'SQL=%',[SQL]);
     ! end;
     - the use of a ISynLog interface will allow you to have an automated
     log entry created when the method is left
     - if TSynLogFamily.HighResolutionTimeStamp is TRUE, high-resolution
     time stamp will be written instead of ISO 8601 date and time: this will
     allow performance profiling of the application on the customer side
     - if you just need to access the log inside the method block, you may
     not need any ISynLog interface:
     ! procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self,'SQLFlush');
     !   // do some stuff
     ! end;
     - if no Method name is supplied, it will use the caller address, and
     will write it as hexa and with full unit and symbol name, if the debugging
     information is available (i.e. if TSynMapFile retrieved the .map content):
     ! procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self);
     !   // do some stuff
     ! end;
     - note that supplying a method name is faster than using the .map content:
     if you want accurate profiling, it's better to use a method name or not to
     use a .map file
     - Enter() will write the class name (and the unit name for classes with
     published properties, if TSynLogFamily.WithUnitName is true) for both
     enter (+) and leave (-) events:
      $ 20110325 19325801  +    MyDBUnit.TMyDB(004E11F4).SQLExecute
      $ 20110325 19325801 info   SQL=SELECT * FROM Table;
      $ 20110325 19325801  -    MyDBUnit.TMyDB(004E11F4).SQLExecute }
    procedure Enter(aInstance: TObject=nil; aMethodName: PUTF8Char=nil); overload;
    {/ to be called and assigned to a ISynLog interface at the beginning
     of a method
     - this is the main method to be called within a class method:
     ! class function TMyDB.SQLValidate(const SQL: RawUTF8): boolean;
     ! var ILog: ISynLog;
     ! begin
     !   ILog := TSynLogDB.Enter(self,'SQLValidate');
     !   // do some stuff
     !   ILog.Log(sllInfo,'SQL=% returned %',[SQL,result]);
     ! end;
     - the use of a ISynLog interface will allow you to have an automated
     log entry created when the method is left
     - if TSynLogFamily.HighResolutionTimeStamp is TRUE, high-resolution
     time stamp will be written instead of ISO 8601 date and time: this will
     allow performance profiling of the application on the customer side
     - if you just need to access the log inside the method block, you may
     not need any ISynLog interface:
     ! class procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self,'SQLFlush');
     !   // do some stuff
     ! end;
     - if no Method name is supplied, it will use the caller address, and
     will write it as hexa and with full unit and symbol name, if the debugging
     information is available (i.e. if TSynMapFile retrieved the .map content):
     ! class procedure TMyDB.SQLFlush;
     ! begin
     !   TSynLogDB.Enter(self);
     !   // do some stuff
     ! end;
     - note that supplying a method name is faster than using the .map content:
     if you want accurate profiling, it's better to use a method name or not to
     use a .map file
     - Enter() will write the class name (and the unit name for classes with
     published properties, if TSynLogFamily.WithUnitName is true) for both
     enter (+) and leave (-) events:
      $ 20110325 19325801  +    MyDBUnit.TMyDB.SQLValidate
      $ 20110325 19325801 info   SQL=SELECT * FROM Table returned 1;
      $ 20110325 19325801  -    MyDBUnit.TMyDB.SQLValidate }
    procedure Enter(aClassType: TClass; aMethodName: PUTF8Char=nil); overload;
    /// retrieve the current instance of this TSynLog class
    // - to be used for direct logging, without any Enter/Leave:
    // ! TSynLogDB.Add.Log(llError,'The % statement didn't work',[SQL]);
    // - to be used for direct logging, without any Enter/Leave (one parameter
    // version - just the same as previous):
    // ! TSynLogDB.Add.Log(llError,'The % statement didn't work',SQL);
    // - is just a wrapper around Family.SynLog - the same code will work:
    // ! TSynLogDB.Family.SynLog.Log(llError,'The % statement didn't work',[SQL]);
  //  class function Add: TSynLog;
  //    {$ifdef HASINLINE}inline;{$endif}
    /// retrieve the family of this TSynLog class type
   // class function Family: TSynLogFamily; overload;
{$ifndef DELPHI5OROLDER}
    /// call this method to add some information to the log at the specified level
    // - % = #37 indicates a string, integer, floating-point, or class parameter
    // to be appended as text (e.g. class name)
    // - $ = #36 indicates an integer to be written with 2 digits and a comma
    // - Ј = #163 indicates an integer to be written with 4 digits and a comma
    // - µ = #181 indicates an integer to be written with 3 digits without any comma
    // - ¤ = #164 indicates CR+LF chars
    // - CR = #13 indicates CR+LF chars
    // - § = #167 indicates to trim last comma
    // - since some of this characters above are > #127, they are not UTF-8
    // ready, so we expect the input format to be WinAnsi, i.e. mostly English
    // text (with chars < #128) with some values to be inserted inside
    // - note that cardinal values should be type-casted to Int64() (otherwise
    // the integer mapped value will be transmitted, therefore wrongly)
    procedure Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArgs: array of const;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one RawUTF8 parameter
    procedure Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArg: RawUTF8;
      aInstance: TObject=nil); overload;
    /// same as Log(Level,TextFmt,[]) but with one Int64 parameter
    procedure Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArg: Int64;
      aInstance: TObject=nil); overload;
{$endif}
    /// call this method to add some information to the log at the specified level
    // - if Instance is set and Text is not '', it will log the corresponding
    // class name and address (to be used e.g. if you didn't call TSynLog.Enter()
    // method first) - for instance
    // ! TSQLLog.Add.Log(sllDebug,'GarbageCollector',GarbageCollector);
    // will append this line to the log:
    // $ 0000000000002DB9 debug TObjectList(00425E68) GarbageCollector
    // - if Instance is set and Text is '', will behave the same as
    // Log(Level,Instance), i.e. write the Instance as JSON content
    procedure Log(Level: TSynLogInfo; const Text: RawUTF8;
      aInstance: TObject=nil); overload;
    /// call this method to add the content of an object to the log at a
    // specified level
    // - this default implementation will just write the class name and its hexa
    // pointer value, and handle TList, TCollections and TStrings - for instance:
    // ! TSynLog.Add.Log(sllDebug,GarbageCollector);
    // will append this line to the log:
    // $ 20110330 10010005 debug {"TObjectList(00B1AD60)":["TObjectList(00B1AE20)","TObjectList(00B1AE80)"]}
    // - if aInstance is an Exception, it will handle its class name and Message:
    // $ 20110330 10010005 debug "EClassName(00C2129A)":"Exception message" 
    // - use TSQLLog from SQLite3Commons unit to add the record content, written
    // as human readable JSON
    procedure Log(Level: TSynLogInfo; aInstance: TObject); overload;
    /// call this method to add the content of most low-level types to the log
    // at a specified level
    // - this overriden implementation will write the value content,
    // written as human readable JSON: handle dynamic arrays and enumerations
    // - TSQLLog from SQLite3Commons unit will be able to write
    // TObject/TSQLRecord and sets content as JSON
    procedure Log(Level: TSynLogInfo; aName: PWinAnsiChar;
      aTypeInfo: pointer; var aValue; Instance: TObject=nil); overload;
    /// call this method to add the caller address to the log at the specified level
    // - if the debugging info is available from TSynMapFile, will log the
    // unit name, associated symbol and source code line
    procedure Log(Level: TSynLogInfo); overload;
    /// the associated logging family
    property GenericFamily: TSynLogFamily read fFamily;
    /// the associated file name containing the log
    // - this is accurate only with the default implementation of the class:
    // any child may override it with a custom logging mechanism
    property FileName: TFileName read fFileName;
  end;

  

  /// used by TSynLogFile to refer to a method profiling in a .log file
  // - i.e. map a sllEnter/sllLeave event in the .log file
  TSynLogFileProc = record
    /// the index of the sllEnter event in the TSynLogFile.fLevels[] array
    Index: cardinal;
    /// the associated time elapsed in this method (in micro seconds)
    // - computed from the sllLeave time difference (high resolution timer)
    Time: cardinal;
    /// the time elapsed in this method and not in nested methods
    // - computed from Time property, minus the nested calls
    ProperTime: cardinal;
  end;

  /// used by TSynLogFile to refer to global method profiling in a .log file
  // - i.e. map all sllEnter/sllLeave event in the .log file
  TSynLogFileProcDynArray = array of TSynLogFileProc;

  TSynLogFileProcArray = array[0..(MaxInt div sizeof(TSynLogFileProc))-1] of TSynLogFileProc;
  PSynLogFileProcArray = ^TSynLogFileProcArray;

  /// used by TSynLogFile.LogProcSort method
  TLogProcSortOrder = (
    soNone, soByName, soByOccurrence, soByTime, soByProperTime);
    
  /// used to parse a .log file, as created by TSynLog, into high-level data
  // - this particular TMemoryMapText class will retrieve only valid event lines
  // (i.e. will fill EventLevel[] for each line <> sllNone)
  // - Count is not the global text line numbers, but the number of valid events
  // within the file (LinePointers/Line/Strings will contain only event lines) -
  // it will not be a concern, since the .log header is parsed explicitely
  TSynLogFile = class(TMemoryMapText)
  protected
    /// map the events occurring in the .log file content
    fLevels: TSynLogInfoDynArray;
    /// as extracted from the .log header
    fExeName, fExeVersion, fHost, fUser, fCPU, fOSDetailed, fInstanceName: RawUTF8;
    fExeDate: TDateTime;
    fOS: TWindowsVersion;
    fOSServicePack: integer;
    fWow64: boolean;
    fStartDateTime: TDateTime;
    /// retrieve all used event levels
    fLevelUsed: TSynLogInfos;
    /// =0 if date time resolution, >0 if high-resolution time stamp
    fFreq: Int64;
    /// used by EventDateTime() to compute date from time stamp
    fFreqPerDay: double;
    /// method profiling data
    fLogProcCurrent: PSynLogFileProcArray;
    fLogProcCurrentCount: integer;
    fLogProcNatural: TSynLogFileProcDynArray;
    fLogProcNaturalCount: integer;
    fLogProcMerged: TSynLogFileProcDynArray;
    fLogProcMergedCount: integer;
    fLogProcIsMerged: boolean;
    fLogProcStack: array of cardinal;
    fLogProcStackCount: integer;
    fLogProcSortInternalOrder: TLogProcSortOrder;
    /// used by ProcessOneLine
    fLogLevelsTextMap: array[TSynLogInfo] of cardinal;
    procedure SetLogProcMerged(const Value: boolean);
    /// retrieve headers + fLevels[] + fLogProcNatural[], and delete invalid fLines[]
    procedure LoadFromMap; override;
    /// compute fLevels[] + fLogProcNatural[] for each .log line during initial reading
    procedure ProcessOneLine(LineBeg, LineEnd: PUTF8Char; max: integer); override;
    /// called by LogProcSort method
    function LogProcSortComp(A,B: Integer): integer;
    procedure LogProcSortInternal(L,R: integer);
  public
    /// retrieve the date and time of an event
    // - returns 0 in case of an invalid supplied index
    function EventDateTime(aIndex: integer): TDateTime;
    /// sort the LogProc[] array according to the supplied order
    procedure LogProcSort(Order: TLogProcSortOrder);
    /// return the number of matching events in the log
    function EventCount(const aSet: TSynLogInfos): integer;
    /// retrieve the level of an event
    // - is calculated by Create() constructor
    // - EventLevel[] array index is from 0 to Count-1
    property EventLevel: TSynLogInfoDynArray read fLevels;
    /// retrieve all used event levels
    // - is calculated by Create() constructor
    property EventLevelUsed: TSynLogInfos read fLevelUsed;
    /// profiled methods information
    // - is calculated by Create() constructor
    // - will contain the sllEnter index, with the associated elapsed time
    // - number of items in the array is retrieved by the LogProcCount property
    property LogProc: PSynLogFileProcArray read fLogProcCurrent;
    /// the current sort order
    property LogProcOrder: TLogProcSortOrder read fLogProcSortInternalOrder;
    /// if the method information must be merged for the same method name
    property LogProcMerged: boolean read fLogProcIsMerged write SetLogProcMerged;
  published
    /// the associated executable name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestSQL3.exe'
    property ExecutableName: RawUTF8 read fExeName;
    /// the associated executable version
    // - returns e.g. '0.0.0.0'
    property ExecutableVersion: RawUTF8 read fExeVersion;
    /// the associated executable build date and time
    property ExecutableDate: TDateTime read fExeDate;
    /// for a library, the associated instance name (with path)
    // - returns e.g. 'C:\Dev\lib\SQLite3\exe\TestLibrary.dll'
    // - for an executable, will be left void
    property InstanceName: RawUTF8 read fInstanceName;
    /// the computer host name in which the process was running on
    property ComputerHost: RawUTF8 read fHost;
    /// the computer user name who launched the process
    property RunningUser: RawUTF8 read fUser;
    /// the computer CPU in which the process was running on
    // - returns e.g. '1*0-15-1027'
    property CPU: RawUTF8 read fCPU;
    /// the computer Operating System in which the process was running on
    property OS: TWindowsVersion read fOS;
    /// the Operating System Service Pack number
    property ServicePack: integer read fOSServicePack;
    /// the computer Operating System in which the process was running on
    // - returns e.g. '2.3=5.1.2600' for Windows XP
    property DetailedOS: RawUTF8 read fOSDetailed;
    /// if the 32 bit process was running under WOW 64 virtual emulation
    property Wow64: boolean read fWow64;
    /// the date and time at which the log file was started
    property StartDateTime: TDateTime read fStartDateTime;
    /// number of profiled methods in this .log file
    // - i.e. number of items in the LogProc[] array
    property LogProcCount: integer read fLogProcCurrentCount;
  end;


const
  /// up to 16 TSynLogFamily, i.e. TSynLog children classes can be defined
  MAX_SYNLOGFAMILY = 15;

  /// can be set to TSynLogFamily.Level in order to log all available events
  LOG_VERBOSE: TSynLogInfos = [succ(sllNone)..high(TSynLogInfo)];

  /// contains the logging levels for which stack trace should be dumped
  // - i.e. when such a log event occur, and the available recursive stack has
  // not been traced yet (if sllEnter if not in the current selected levels)
  LOG_STACKTRACE: TSynLogInfos = [sllError,sllException,sllExceptionOS];

  /// the text equivalency of each logging level, as written in the log file
  // - PCardinal(@LOG_LEVEL_TEXT[L][3])^ will be used for fast level matching
  // so text must be unique for characters [3..6] -> e.g. 'UST4'
  LOG_LEVEL_TEXT: array[TSynLogInfo] of string[7] = (
    '       ', ' info  ', ' event ', ' debug ', ' PrtWr ', ' PrtRd ', ' trace ', ' warn  ', ' ERROR ',
    '  +    ', '  -    ',
    ' OSERR ', ' EXC   ', ' EXCOS ', ' mem   ', ' stack ', ' fail  ',
    ' SQL   ', ' cache ', ' res   ', ' DB    ', ' http  ', ' clnt  ', ' srvr  ',
    ' call  ', ' ret   ', ' auth  ', ' cust2 ');

  /// the "magic" number used to identify .log.synlz compressed files, as
  // created by TSynLogFamily.EventArchiveSynLZ
  LOG_MAGIC = $ABA51051;

//var
  /// the kind of .log file generated by TSynTestsLogged
  //TSynLogTestLog: TSynLogClass = TSynLog;


/// compress a data content using the SynLZ algorithm from one stream into another
// - returns the number of bytes written to Dest
// - you should specify a Magic number to be used to identify the block
//function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream; Magic: cardinal): integer; overload;

/// uncompress using the SynLZ algorithm from one stream into another
// - returns a newly create memory stream containing the uncompressed data
// - returns nil if source data is invalid
// - you should specify a Magic number to be used to identify the block
// - this function will also recognize the block at the end of the source stream
// (if was appended to an existing data - e.g. a .mab at the end of a .exe)
// - on success, Source will point after all read data (so that you can e.g.
// append several data blocks to the same stream)
function StreamUnSynLZ(Source: TStream; Magic: cardinal): TMemoryStream; overload;

/// uncompress using the SynLZ algorithm from one file into another
// - returns a newly create memory stream containing the uncompressed data
// - returns nil if source file is invalid (e.g. invalid name or invalid content)
// - you should specify a Magic number to be used to identify the block
// - this function will also recognize the block at the end of the source file
// (if was appended to an existing data - e.g. a .mab at the end of a .exe)
function StreamUnSynLZ(const Source: TFileName; Magic: cardinal): TMemoryStream; overload;

/// compress a file content using the SynLZ algorithm a file content
// - source file is split into 128 MB blocks for fast in-memory compression of
// any file size
// - you should specify a Magic number to be used to identify the compressed
// file format
function FileSynLZ(const Source, Dest: TFileName; Magic: Cardinal): boolean;

/// compress a file content using the SynLZ algorithm a file content
// - you should specify a Magic number to be used to identify the compressed
// file format
function FileUnSynLZ(const Source, Dest: TFileName; Magic: Cardinal): boolean;

/// a TSynLogArchiveEvent handler which will delete older .log files
function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;

/// a TSynLogArchiveEvent handler which will compress older .log files
// using our proprietary SynLZ format
// - resulting file will have the .synlz extension and will be located
// in the aDestinationPath directory, i.e. TSynLogFamily.ArchivePath+'\log\YYYYMM\'
// - use UnSynLZ.dpr tool to uncompress it into .log textual file
// - SynLZ is much faster than zip for compression content, but proprietary
function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;


resourcestring
  sInvalidIPAddress = '"%s" is an invalid IP v4 address';
  sInvalidEmailAddress = '"%s" is an invalid email address';
  sInvalidPattern = '"%s" does not match the expected pattern';
  sCharacter01n = 'character,character,characters';
  sInvalidTextLengthMin = 'Expect at least %d %s';
  sInvalidTextLengthMax = 'Expect up to %d %s';
  sInvalidTextChar = 'Expect at least %d %s %s,Expect up to %d %s %s,'+
    'alphabetical,digital,punctuation,lowercase,uppercase,space,'+
    'Too much spaces on the left,Too much spaces on the right';
  sValidationFailed = '"%s" rule failed';
  sValidationFieldVoid = 'An unique key field must not be void';
  sValidationFieldDuplicate = 'Value already used for this unique key field';


implementation


{ ************ some fast UTF-8 / Unicode / Ansi conversion routines }

var
  // internal list of TSynAnsiConvert instances
  SynAnsiConvertList: TObjectList = nil;


{ TSynAnsiConvert }

const
  DefaultChar: AnsiChar = '?';

function TSynAnsiConvert.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: Cardinal): PWideChar;
var c: cardinal;
begin
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  if SourceChars>=4 then
  repeat
    c := pCardinal(Source)^;
    if c and $80808080<>0 then
      break; // break on first non ASCII quad
    dec(SourceChars,4);
    inc(Source,4);
    pCardinal(Dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
    c := c shr 16;
    pCardinal(Dest+2)^ := (c shl 8 or c) and $00ff00ff;
    inc(Dest,4);
  until SourceChars<4;
  if (SourceChars>0) and (ord(Source^)<128) then
  repeat
    dec(SourceChars);
    Dest^ := WideChar(ord(Source^));
    inc(Source);
    inc(Dest);
  until (SourceChars=0) or (ord(Source^)>=128);
  // rely on the Operating System for all remaining ASCII characters
  if SourceChars=0 then
    result := Dest else
    result := Dest+MultiByteToWideChar(fCodePage,0,Source,SourceChars,Dest,SourceChars);
  result^ := #0;
end;

function TSynAnsiConvert.AnsiBufferToUTF8(Dest: PUTF8Char;
  Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char;
var tmp: array[byte] of WideChar;
    c: cardinal;
    U: PWideChar;
begin
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  if SourceChars>=4 then
  repeat
    c := pCardinal(Source)^;
    if c and $80808080<>0 then
      break; // break on first non ASCII quad
    pCardinal(Dest)^ := c;
    dec(SourceChars,4);
    inc(Source,4);
    inc(Dest,4);
  until SourceChars<4;
  if (SourceChars>0) and (ord(Source^)<128) then
  repeat
    Dest^ := Source^;
    dec(SourceChars);
    inc(Source);
    inc(Dest);
  until (SourceChars=0) or (ord(Source^)>=128);
  // rely on the Operating System for all remaining ASCII characters
  if SourceChars=0 then
    result := Dest else
    if SourceChars<SizeOf(tmp) shr 1 then
      result := Dest+RawUnicodeToUTF8(Dest,SourceChars*3,tmp,
        (PtrUInt(AnsiBufferToUnicode(tmp,Source,SourceChars))-PtrUInt(@tmp))shr 1) else begin
      GetMem(U,SourceChars*2+2);
      result := Dest+RawUnicodeToUtf8(Dest,SourceChars*3,U,
        AnsiBufferToUnicode(U,Source,SourceChars)-U);
      FreeMem(U);
    end;
  result^ := #0;
end;

function TSynAnsiConvert.AnsiToRawUnicode(const AnsiText: RawByteString): RawUnicode;
begin
  result := AnsiToRawUnicode(pointer(AnsiText),length(AnsiText));
end;

function TSynAnsiConvert.AnsiToRawUnicode(Source: PAnsiChar; SourceChars: Cardinal): RawUnicode;
var U: PWideChar;
begin
  if SourceChars=0 then
    result := '' else begin
    SetString(result,nil,SourceChars*2+1);
    U := AnsiBufferToUnicode(pointer(result),Source,SourceChars);
    U^ := #0;
    SetLength(result,PtrUInt(U)-PtrUInt(result)+1);
  end;
end;

function TSynAnsiConvert.AnsiToUnicodeString(Source: PAnsiChar;
  SourceChars: Cardinal): SynUnicode;
begin
  result := '';
  if SourceChars<>0 then begin
    SetLength(result,SourceChars);
    SetLength(result,AnsiBufferToUnicode(pointer(result),Source,SourceChars)-pointer(result));
  end;
end;

function TSynAnsiConvert.AnsiToUTF8(const AnsiText: RawByteString): RawUTF8;
begin
  result := AnsiBufferToRawUTF8(pointer(AnsiText),length(AnsiText));
end;

function TSynAnsiConvert.AnsiBufferToRawUTF8(Source: PAnsiChar; SourceChars: Cardinal): RawUTF8;
var tmpU8: array[0..256*3] of AnsiChar;
    U8: PUTF8Char;
begin
  if SourceChars=0 then
    result := '' else
  if SourceChars<SizeOf(tmpU8) div 3 then
    SetString(result,tmpU8,AnsiBufferToUTF8(tmpU8,Source,SourceChars)-tmpU8) else begin
    Getmem(U8,SourceChars*3);
    SetString(result,U8,AnsiBufferToUTF8(U8,Source,SourceChars)-U8);
    FreeMem(U8);
  end;
end;

constructor TSynAnsiConvert.Create(aCodePage: integer);
begin
  fCodePage := aCodePage;
end;

function IsFixedWidthCodePage(aCodePage: integer): boolean;
begin
  result := (aCodePage>=1250) and (aCodePage<=1257);
end;

class function TSynAnsiConvert.Engine(aCodePage: integer): TSynAnsiConvert;
var i: integer;
begin
  if SynAnsiConvertList=nil then begin
    SynAnsiConvertList := TObjectList.Create;
    GarbageCollector.Add(SynAnsiConvertList); // global list
  end else
    for i := 0 to SynAnsiConvertList.Count-1 do begin
      result := SynAnsiConvertList.List[i];
      if result.CodePage=aCodePage then
        exit;
    end;
  if IsFixedWidthCodePage(aCodePage) then
    result := TSynAnsiFixedWidth.Create(aCodePage) else
    result := TSynAnsiConvert.Create(aCodePage);
  SynAnsiConvertList.Add(result);
end;

function TSynAnsiConvert.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: Cardinal): PAnsiChar;
var c: cardinal;
begin
  // first handle trailing 7 bit ASCII chars, by pairs (Sha optimization)
  if SourceChars>=2 then
  repeat
    c := PCardinal(Source)^;
    if c and $ff80ff80<>0 then
      break; // break on first non ASCII pair
    dec(SourceChars,2);
    inc(Source,2);
    c := c shr 8 or c;
    pWord(Dest)^ := c;
    inc(Dest,2);
  until SourceChars<2;
  if (SourceChars>0) and (ord(Source^)<128) then
  repeat
    Dest^ := AnsiChar(ord(Source^));
    dec(SourceChars);
    inc(Source);
    inc(Dest);
  until (SourceChars=0) or (ord(Source^)>=128);
  // rely on the Operating System for all remaining ASCII characters
  if SourceChars=0 then
    result := Dest else
    result := Dest+WideCharToMultiByte(
      fCodePage,0,Source,SourceChars,Dest,SourceChars*3,@DefaultChar,nil);
end;

function TSynAnsiConvert.UTF8BufferToAnsi(Dest: PAnsiChar;
  Source: PUTF8Char; SourceChars: Cardinal): PAnsiChar;
var tmp: array[0..256*2-1] of WideChar;
    U: PWideChar;
begin
  if SourceChars<SizeOf(tmp) shr 1 then
    result := UnicodeBufferToAnsi(Dest,tmp,UTF8ToWideChar(tmp,Source,SourceChars) shr 1) else begin
    Getmem(U,SourceChars*2+1);
    result := UnicodeBufferToAnsi(Dest,U,UTF8ToWideChar(U,Source,SourceChars) shr 1);
    Freemem(U);
  end;
end;

function TSynAnsiConvert.UTF8BufferToAnsi(Source: PUTF8Char; SourceChars: Cardinal): RawByteString;
var tmpA: array[byte] of AnsiChar;
    A: PAnsiChar;
begin
  if SourceChars=0 then
    result := '' else begin
    if SourceChars<SizeOf(tmpA) then
      SetString(result,tmpA,Utf8BufferToAnsi(tmpA,Source,SourceChars)-tmpA) else begin
      Getmem(A,SourceChars+1);
      SetString(result,A,Utf8BufferToAnsi(A,Source,SourceChars)-A);
      FreeMem(A);
    end;
{$ifdef UNICODE}
    PWord(PtrInt(result)-12)^ := fCodePage; // force set code page
{$endif}
  end;
end;

function TSynAnsiConvert.UTF8ToAnsi(const UTF8: RawUTF8): RawByteString;
begin
  result := UTF8BufferToAnsi(pointer(UTF8),length(UTF8));
end;

function TSynAnsiConvert.UnicodeBufferToAnsi(Source: PWideChar; SourceChars: Cardinal): RawByteString;
var tmpA: array[byte] of AnsiChar;
    A: PAnsiChar;
begin
  if SourceChars=0 then
    result := '' else begin
    if SourceChars<SizeOf(tmpA) then
      SetString(result,tmpA,UnicodeBufferToAnsi(tmpA,Source,SourceChars)-tmpA) else begin
      Getmem(A,SourceChars+1);
      SetString(result,A,UnicodeBufferToAnsi(A,Source,SourceChars)-A);
      FreeMem(A);
    end;
{$ifdef UNICODE}
    PWord(PtrInt(result)-12)^ := fCodePage; // force set code page
{$endif}
  end;
end;

function TSynAnsiConvert.RawUnicodeToAnsi(const Source: RawUnicode): RawByteString;
begin
  result := UnicodeBufferToAnsi(pointer(Source),length(Source) shr 1);
end;

function TSynAnsiConvert.AnsiToAnsi(From: TSynAnsiConvert; const Source: RawByteString): RawByteString;
begin
  if From=self then
    result := Source else
    result := AnsiToAnsi(From,pointer(Source),length(Source));
end;

function TSynAnsiConvert.AnsiToAnsi(From: TSynAnsiConvert; Source: PAnsiChar; SourceChars: cardinal): RawByteString; 
var tmpU: array[byte] of WideChar;
    U: PWideChar;
begin
  if From=self then
    SetString(result,Source,SourceChars) else
  if SourceChars<sizeof(tmpU) shr 1 then
    result := UnicodeBufferToAnsi(tmpU,
      (PtrUInt(From.AnsiBufferToUnicode(tmpU,Source,SourceChars))-PtrUInt(@tmpU))shr 1) else begin
    GetMem(U,SourceChars*2+2);
    result := UnicodeBufferToAnsi(U,From.AnsiBufferToUnicode(U,Source,SourceChars)-U);
    FreeMem(U);
  end;
end;


{ TSynAnsiFixedWidth }

function TSynAnsiFixedWidth.AnsiBufferToUnicode(Dest: PWideChar;
  Source: PAnsiChar; SourceChars: Cardinal): PWideChar;
var i: Integer;
begin
  for i := 1 to SourceChars shr 2 do begin
    Dest[0] := WideChar(fAnsiToWide[Ord(Source[0])]);
    Dest[1] := WideChar(fAnsiToWide[Ord(Source[1])]);
    Dest[2] := WideChar(fAnsiToWide[Ord(Source[2])]);
    Dest[3] := WideChar(fAnsiToWide[Ord(Source[3])]);
    inc(Source,4);
    inc(Dest,4);
  end;
  for i := 1 to SourceChars and 3 do begin
    Dest^ := WideChar(fAnsiToWide[Ord(Source^)]);
    inc(Dest);
    inc(Source);
  end;
  result := Dest;
end;

function TSynAnsiFixedWidth.AnsiBufferToUTF8(Dest: PUTF8Char;
  Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char;
var EndSource, EndSourceBy4: PAnsiChar;
    c: Cardinal;
label By4, By1; // ugly but faster
begin
  if (self=nil) or (Dest=nil) then begin
    Result := nil;
    Exit;
  end else
  if (Source<>nil) and (SourceChars>0) then begin
    // first handle 7 bit ASCII WideChars, by quads (Sha optimization)
    EndSource := Source+SourceChars;
    EndSourceBy4 := EndSource-4;
    if (PtrUInt(Source) and 3=0) and (Source<=EndSourceBy4) then
    repeat
By4:  c := pCardinal(Source)^;
      if c and $80808080<>0 then
        goto By1; // break on first non ASCII quad
      inc(Source,4);
      pCardinal(Dest)^ := c;
      inc(Dest,4);
    until Source>EndSourceBy4;
    // generic loop, handling one WideChar per iteration
    if Source<EndSource then
    repeat
By1:  c := byte(Source^); inc(Source);
      if c<=$7F then begin
        Dest^ := AnsiChar(c); // 0..127 don't need any translation
        Inc(Dest);
        if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
        if Source<endSource then continue else break;
      end
      else begin
        c := fAnsiToWide[c]; // convert WinAnsi char into Unicode char
        if c>$7ff then begin
          Dest[0] := AnsiChar($E0 or (c shr 12));
          Dest[1] := AnsiChar($80 or ((c shr 6) and $3F));
          Dest[2] := AnsiChar($80 or (c and $3F));
          Inc(Dest,3);
          if (PtrUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4 else
          if Source<EndSource then continue else break;
        end else begin
          Dest[0] := AnsiChar($C0 or (c shr 6));
          Dest[1] := AnsiChar($80 or (c and $3F));
          Inc(Dest,2);
          if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
          if Source<endSource then continue else break;
        end;
      end;
    until false;
  end;
  Dest^ := #0;
  Result := Dest;
end;

function TSynAnsiFixedWidth.AnsiToRawUnicode(Source: PAnsiChar; SourceChars: Cardinal): RawUnicode;
begin
  if SourceChars=0 then
    result := '' else begin
    SetString(result,nil,SourceChars*2+1);
    AnsiBufferToUnicode(pointer(result),Source,SourceChars)^ := #0;
  end;
end;

constructor TSynAnsiFixedWidth.Create(aCodePage: integer);
var i: integer;
    PW: PWideChar;
    A256: array[0..256] of AnsiChar;
    U256: array[0..256] of WideChar; // AnsiBufferToUnicode() write a last #0
begin
  inherited;
  if IsFixedWidthCodePage(aCodePage) then begin
    // create internal look-up tables from Operating System returned values
    SetLength(fAnsiToWide,256);
    for i := 0 to 255 do
      A256[i] := AnsiChar(i);
    PW := inherited AnsiBufferToUnicode(U256,A256,256);
    Assert(PtrInt(PW)-PtrInt(@U256)=256*2);
    move(U256,fAnsiToWide[0],256*2);
    SetLength(fWideToAnsi,65536);
    fillchar(fWideToAnsi[1],65535,ord('?')); // '?' for unknown char
    for i := 1 to 255 do
      if fAnsiToWide[i]<>0 then
        fWideToAnsi[fAnsiToWide[i]] := i;
  end else
    raise ESynException.CreateFmt('%s.Create - Invalid code page %d',
      [ClassName,fCodePage]);
end;

function TSynAnsiFixedWidth.IsValidAnsi(WideText: PWideChar; Length: integer): boolean;
var i: integer;
    wc: cardinal;
begin
  result := false;
  if WideText<>nil then
    for i := 0 to Length-1 do begin
      wc := cardinal(WideText[i]);
      if wc=0 then
        break else
      if wc<256 then
        if fAnsiToWide[wc]<256 then
          continue else
          exit else
          if fWideToAnsi[wc]=ord('?') then
            exit else
            continue;
    end;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsi(WideText: PWideChar): boolean;
var wc: cardinal;
begin
  result := false;
  if WideText<>nil then
    repeat
      wc := cardinal(WideText^);
      inc(WideText);
      if wc=0 then
        break else
      if wc<256 then
        if fAnsiToWide[wc]<256 then
          continue else
          exit else
          if fWideToAnsi[wc]=ord('?') then
            exit else
            continue;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsiU(UTF8Text: PUTF8Char): boolean;
var c: cardinal;
begin
  result := false;
  if UTF8Text<>nil then
    repeat
      c := byte(UTF8Text^); inc(UTF8Text);
      if c=0 then break else
      if c and $80=0 then
        continue else begin
        if UTF8Text^=#0 then break;
        if c and $20=0 then begin
          c := c shl 6+byte(UTF8Text^)-$00003080; inc(UTF8Text);
        end else begin
          c := c shl 6+byte(UTF8Text^); inc(UTF8Text);
          if UTF8Text^=#0 then break;
          c := c shl 6+byte(UTF8Text^)-$000E2080; inc(UTF8Text);
        end;
        if fWideToAnsi[c]=ord('?') then
          exit; // invalid char in the WinAnsi code page
      end;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.IsValidAnsiU8Bit(UTF8Text: PUTF8Char): boolean;
var c: Cardinal;
begin
  result := false;
  if UTF8Text<>nil then
    repeat
      c := byte(UTF8Text^); inc(UTF8Text);
      if c=0 then break else
      if c and $80=0 then
        continue else begin
        if UTF8Text^=#0 then break;
        if c and $20=0 then begin
          c := c shl 6+byte(UTF8Text^)-$00003080; inc(UTF8Text);
        end else begin
          c := c shl 6+byte(UTF8Text^); inc(UTF8Text);
          if UTF8Text^=#0 then break;
          c := c shl 6+byte(UTF8Text^)-$000E2080; inc(UTF8Text);
        end;
        if (c>255) or (fAnsiToWide[c]>255) then
          exit; // not 8 bit char (like "tm" or such) is marked invalid
      end;
    until false;
  result := true;
end;

function TSynAnsiFixedWidth.UnicodeBufferToAnsi(Dest: PAnsiChar;
  Source: PWideChar; SourceChars: Cardinal): PAnsiChar;
var c: cardinal;
begin
  // first handle trailing 7 bit ASCII chars, by pairs (Sha optimization)
  if SourceChars>=2 then
  repeat
    c := PCardinal(Source)^;
    if c and $ff80ff80<>0 then
      break; // break on first non ASCII pair
    dec(SourceChars,2);
    inc(Source,2);
    c := c shr 8 or c;
    pWord(Dest)^ := c;
    inc(Dest,2);
  until SourceChars<2;
  // use internal lookup tables for fast process of remaining chars
  for c := 1 to SourceChars shr 2 do begin
    Dest[0] := AnsiChar(fWideToAnsi[Ord(Source[0])]);
    Dest[1] := AnsiChar(fWideToAnsi[Ord(Source[1])]);
    Dest[2] := AnsiChar(fWideToAnsi[Ord(Source[2])]);
    Dest[3] := AnsiChar(fWideToAnsi[Ord(Source[3])]);
    inc(Source,4);
    inc(Dest,4);
  end;
  for c := 1 to SourceChars and 3 do begin
    Dest^ := AnsiChar(fWideToAnsi[Ord(Source^)]);
    inc(Dest);
    inc(Source);
  end;
  result := Dest;
end;

function TSynAnsiFixedWidth.UTF8BufferToAnsi(Dest: PAnsiChar;
  Source: PUTF8Char; SourceChars: Cardinal): PAnsiChar;
var c: cardinal;
    endSource, endSourceBy4: PUTF8Char;
label By1, By4; // ugly but faster
begin
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  endSource := Source+SourceChars;
  endSourceBy4 := endSource-4;
  if (PtrUInt(Source) and 3=0) and (Source<=endSourceBy4) then
  repeat
By4:c := pCardinal(Source)^;
    if c and $80808080<>0 then
      goto By1; // break on first non ASCII quad
    pCardinal(Dest)^ := c;
    inc(Source,4);
    inc(Dest,4);
  until Source>endSourceBy4;
  // generic loop, handling one UTF-8 code per iteration
  if Source<endSource then
  repeat
By1:c := byte(Source^); inc(Source);
    if ord(c) and $80=0 then begin
      Dest^ := AnsiChar(c);
      inc(Dest);
      if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
      if Source<endSource then continue else break;
    end else begin
      if Source>=endSource then break;
      if c and $20=0 then begin
        c := c shl 6+byte(Source^)-$00003080; inc(Source);
      end else begin
        c := c shl 6+byte(Source^); inc(Source);
        if Source>=endSource then break;
        c := c shl 6+byte(Source^)-$000E2080; inc(Source);
      end;
      Dest^ := AnsiChar(fWideToAnsi[c]);
      inc(Dest);
      if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
      if Source<endSource then continue else break;
    end;
  until false;
  result := Dest;
end;

function TSynAnsiFixedWidth.WideCharToAnsiChar(wc: cardinal): integer;
begin
  if wc<256 then
    if fAnsiToWide[wc]<256 then
      result := wc else
      result := -1 else
      if wc<=65535 then begin
        result := fWideToAnsi[wc];
        if result=ord('?') then
          result := -1;
      end else
      result := -1;
end;


function UnicodeCharToUtf8(Dest: PUTF8Char; aWideChar: PtrUInt): integer;
begin
  if aWideChar<=$7F then begin
    Dest^ := AnsiChar(aWideChar);
    result := 1;
  end else
  if aWideChar>$7ff then begin
    Dest[0] := AnsiChar($E0 or (aWideChar shr 12));
    Dest[1] := AnsiChar($80 or ((aWideChar shr 6) and $3F));
    Dest[2] := AnsiChar($80 or (aWideChar and $3F));
    result := 3;
  end else begin
    Dest[0] := AnsiChar($C0 or (aWideChar shr 6));
    Dest[1] := AnsiChar($80 or (aWideChar and $3F));
    result := 2;
  end;
end;

function WinAnsiBufferToUtf8(Dest: PUTF8Char; Source: PAnsiChar; SourceChars: Cardinal): PUTF8Char;
begin
  result := WinAnsiConvert.AnsiBufferToUTF8(Dest,Source,SourceChars);
end;

procedure WinAnsiToUnicodeBuffer(const S: WinAnsiString; Dest: PWordArray; DestLen: integer);
var L: PtrInt;
begin
  L := length(S);
  if L<>0 then begin
    if L>=DestLen then
      L := DestLen-1; // truncate to avoid buffer overflow
    WinAnsiConvert.AnsiBufferToUnicode(PWideChar(Dest),pointer(S),L); // include last #0
  end else
    Dest^[0] := 0;
end;

function WinAnsiToRawUnicode(const S: WinAnsiString): RawUnicode;
begin
  result := WinAnsiConvert.AnsiToRawUnicode(S);
end;

function WinAnsiToUtf8(const S: WinAnsiString): RawUTF8;
begin
  result := WinAnsiConvert.AnsiBufferToRawUTF8(pointer(S),length(s));
end;

function WinAnsiToUtf8(WinAnsi: PAnsiChar; WinAnsiLen: integer): RawUTF8; 
begin
  result := WinAnsiConvert.AnsiBufferToRawUTF8(WinAnsi,WinAnsiLen);
end;

function WideCharToWinAnsiChar(wc: cardinal): AnsiChar;
begin
  wc := WinAnsiConvert.WideCharToAnsiChar(wc);
  if integer(wc)=-1 then
    result := '?' else
    result := AnsiChar(wc);
end;

function WideCharToWinAnsi(wc: cardinal): integer;
begin
  result := WinAnsiConvert.WideCharToAnsiChar(wc);
end;

function IsWinAnsi(WideText: PWideChar; Length: integer): boolean;
begin
  result := WinAnsiConvert.IsValidAnsi(WideText,Length);
end;

function IsAnsiCompatible(PC: PAnsiChar): boolean;
begin
  result := false;
  if PC<>nil then
  while true do
    if PC^=#0 then
      break else
    if PC^<=#127 then
      inc(PC) else // 7 bits chars are always OK, whatever codepage/charset is used
      exit;
  result := true;
end;

function IsAnsiCompatible(PC: PAnsiChar; Len: integer): boolean;
var i: integer;
begin
  result := false;
  if PC<>nil then begin
    for i := 1 to Len shr 2 do
      if PCardinal(PC)^ and $80808080<>0 then
        exit else
        inc(PC,4);
    for i := 0 to (Len and 3)-1 do
      if PC[i]>=#127 then
        exit;
  end;
  result := true;
end;

function IsAnsiCompatible(const Text: RawByteString): boolean; overload;
begin
  result := IsAnsiCompatible(PAnsiChar(pointer(Text)),length(Text));
end;

function IsAnsiCompatible(PW: PWideChar): boolean; overload;
begin
  result := false;
  if PW<>nil then
  while true do
    if ord(PW^)=0 then
      break else
    if ord(PW^)<=127 then
      inc(PW) else // 7 bits chars are always OK, whatever codepage/charset is used
      exit;
  result := true;
end;

function IsAnsiCompatible(PW: PWideChar; Len: integer): boolean; overload;
var i: integer;
begin
  result := false;
  if PW<>nil then
    for i := 0 to Len-1 do
      if ord(PW[i])>127 then
        exit;
  result := true;
end;

function IsWinAnsi(WideText: PWideChar): boolean;
begin
  result := WinAnsiConvert.IsValidAnsi(WideText);
end;

function IsWinAnsiU(UTF8Text: PUTF8Char): boolean;
begin
  result := WinAnsiConvert.IsValidAnsiU(UTF8Text);
end;

function IsWinAnsiU8Bit(UTF8Text: PUTF8Char): boolean;
begin
  result := WinAnsiConvert.IsValidAnsiU8Bit(UTF8Text);
end;

function UTF8ToWinPChar(dest: PAnsiChar; source: PUTF8Char; count: integer): integer;
begin
  result := WinAnsiConvert.UTF8BufferToAnsi(dest,source,count)-dest;
end;

procedure UTF8ToShortString(var dest: shortstring; source: PUTF8Char);
var c: cardinal;
    len: integer;
begin
  len := 0;
  if source<>nil then
  repeat
    c := byte(source^); inc(source);
    if c=0 then break else
    if c and $80=0 then begin
      inc(len); dest[len] := AnsiChar(c);
      if len<255 then continue else break;
    end else begin
      if source^=#0 then break;
      if c and $20=0 then begin
        c := c shl 6+byte(source^)-$00003080; inc(source);
      end else begin
        c := c shl 6+byte(source^); inc(source);
        if source^=#0 then break;
        c := c shl 6+byte(source^)-$000E2080; inc(source);
      end;
      // #256.. -> slower but accurate conversion
      inc(len);
      dest[len] := AnsiChar(WinAnsiConvert.WideCharToAnsiChar(c));
      if len<255 then continue else break;
    end;
  until false;
  dest[0] := AnsiChar(len);
end;

function Utf8ToWinAnsi(const S: RawUTF8): WinAnsiString;
begin
  result := WinAnsiConvert.UTF8ToAnsi(S);
end;

function Utf8ToWinAnsi(P: PUTF8Char): WinAnsiString;
begin
  result := WinAnsiConvert.UTF8ToAnsi(P);
end;

procedure Utf8ToRawUTF8(P: PUTF8Char; var result: RawUTF8);
begin // fast and Delphi 2009+ ready
  SetString(result,PAnsiChar(P),StrLen(P));
end;

function UTF8ToWideChar(dest: PWideChar; source: PUTF8Char; MaxDestChars, sourceBytes: PtrInt): PtrInt;
// faster than System.Utf8ToUnicode()
var c: cardinal;
    begd: pWideChar;
    endSource: PUTF8Char;
    endDest: PWideChar;
begin
  result := 0;
  if (source=nil) or (dest=nil) or (sourceBytes=0) then
   exit;
  endSource := source+sourceBytes;
  endDest := dest+MaxDestChars;
  begd := dest;
  repeat
    c := byte(source^);
    inc(source);
    if c and $80=0 then begin
      dest^ := WideChar(c);
      inc(dest);
      if (source<endsource) and (dest<endDest) then
        continue else
        break;
    end else
      if source>=endsource then
        break;
      if c and $20=0 then begin
        c := c shl 6+byte(source^)-$00003080;
        inc(source);
        dest^ := WideChar(c);
        inc(dest);
        if (source>=endsource) or (dest>=endDest) then
          break;
      end else begin
        c := c shl 6+byte(source^);
        inc(source);
        if source>=endsource then
          break;
        c := c shl 6+byte(source^)-$000E2080;
        inc(source);
        dest^ := WideChar(c);
        inc(dest);
        if (source>=endsource) or (dest>=endDest) then
          break;
      end;
  until false;
  dest^ := #0; // always append a WideChar(0) to the end of the buffer
  result := PtrUInt(dest)-PtrUInt(begd); // dest-begd return char length
end;

function UTF8ToWideChar(dest: pWideChar; source: PUTF8Char; sourceBytes: PtrInt=0): PtrInt;
// faster than System.UTF8Decode()
var c: cardinal;
    begd: pWideChar;
    endSource: PUTF8Char;
begin
  result := 0;
  if source=nil then
   exit;
  if sourceBytes=0 then begin
    if source^=#0 then
      exit;
    sourceBytes := StrLen(source);
  end;
  begd := dest;
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  endSource := source+sourceBytes-4;
  if source<=endSource then
  repeat
    c := pCardinal(source)^;
    if c and $80808080<>0 then
      break; // break on first non ASCII quad
    inc(source,4);
    pCardinal(dest)^ := (c shl 8 or (c and $FF)) and $00ff00ff;
    c := c shr 16;
    pCardinal(dest+2)^ := (c shl 8 or c) and $00ff00ff;
    inc(dest,4);
  until source>endSource;
  // generic loop, handling one UTF-8 code per iteration
  inc(endSource,4);
  if source<endSource then
  repeat
    c := byte(source^);
    inc(source);
    if c and $80=0 then begin
      dest^ := WideChar(c);
      inc(dest);
      if source<endsource then
        continue else
        break;
    end else
      if source>=endsource then
        break;
      if c and $20=0 then begin
        c := c shl 6+byte(source^)-$00003080;
        inc(source);
        dest^ := WideChar(c);
        inc(dest);
        if source>=endsource then
          break;
      end else begin
        c := c shl 6+byte(source^);
        inc(source);
        if source>=endsource then
          break;
        c := c shl 6+byte(source^)-$000E2080;
        inc(source);
        dest^ := WideChar(c);
        inc(dest);
        if source>=endsource then
          break;
      end;
  until false;
  dest^ := #0; // always append a WideChar(0) to the end of the buffer
  result := PtrUInt(dest)-PtrUInt(begd); // dest-begd return char length
end;

function Utf8ToUnicodeLength(source: PUTF8Char): PtrUInt;
var c: byte;
begin
  result := 0;
  if source<>nil then
  repeat
    c := byte(source^); inc(source);
    if c=0 then break else
    if c and $80=0 then
      inc(result) else begin
      if source^=#0 then break else inc(source);
      if c and $20<>0 then
        if source^=#0 then break else inc(source);
      inc(result);
    end;
  until false;
end;

function Utf8FirstLineToUnicodeLength(source: PUTF8Char): PtrInt;
var c: byte;
begin
  result := 0;
  if source<>nil then
  repeat
    c := byte(source^); inc(source);
    if c in [0,10,13] then break else // #0, #10 or #13 stop the count
    if c and $80=0 then
      inc(result) else begin
      if source^=#0 then break else inc(source);
      if c and $20<>0 then
        if source^=#0 then break else inc(source);
      inc(result);
    end;
  until false;
end;

function Utf8DecodeToRawUnicode(P: PUTF8Char; L: integer): RawUnicode; overload;
var tmp: RawByteString;
    short: array[byte] of WideChar;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if L=0 then
    L := StrLen(P);
  if L=0 then
    exit;
  // +1 below is for #0 ending -> true WideChar(#0) ending
  if L<sizeof(short) shr 1 then // mostly avoid tmp memory allocation on heap
    SetString(result,PAnsiChar(@short),UTF8ToWideChar(short,P,L)+1) else begin
    SetLength(tmp,L*2+1); // maximum posible unicode size (if all <#128)
    SetString(result,PAnsiChar(pointer(tmp)),UTF8ToWideChar(pointer(tmp),P,L)+1);
  end;
end;

function Utf8DecodeToRawUnicode(const S: RawUTF8): RawUnicode; overload;
begin
  if S='' then
    result := '' else
    result := Utf8DecodeToRawUnicode(pointer(S),PInteger(PtrInt(S)-4)^);
end;

function Utf8DecodeToRawUnicodeUI(const S: RawUTF8; DestLen: PInteger=nil): RawUnicode;
var L: integer;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if S='' then begin
    if DestLen<>nil then
      DestLen^ := 0;
    exit;
  end;
  L := PPtrInt(PtrInt(S)-4)^;
  SetLength(result,L*2+1);
{$ifdef UNICODE2} // not needed: SetLength() did already set the codepage
  PWord(PtrInt(result)-12)^ := CP_UTF16; 
{$endif}
  L := UTF8ToWideChar(pointer(result),Pointer(S),L);
  if DestLen<>nil then
    DestLen^ := L;
end;

function RawUnicodeToUtf8(Dest: PUTF8Char; DestLen: PtrInt; Source: PWideChar; SourceLen: PtrInt): PtrInt; overload;
var c: Cardinal;
    Tail: PWideChar;
begin
  result := PtrInt(Dest);
  if (Source<>nil) and (Dest<>nil) then begin
    // first handle 7 bit ASCII WideChars, by pairs (Sha optimization)
    SourceLen := SourceLen*2+PtrInt(Source);
    Tail := PWideChar(SourceLen)-2;
    if Source<=Tail then
    repeat
      c := PCardinal(Source)^;
      if c and $ff80ff80<>0 then
        break; // break on first non ASCII pair
      inc(Source,2);
      c := c shr 8 or c;
      pWord(Dest)^ := c;
      inc(Dest,2);
    until Source>Tail;
    // generic loop, handling one WideChar per iteration
    Inc(DestLen,PtrInt(Dest));
    if (PtrInt(Dest)<DestLen) and (PtrInt(Source)<SourceLen) then
    repeat
      c := Cardinal(Source^);
      inc(Source);
      if c<=$7F then begin
        Dest^ := AnsiChar(c);
        inc(Dest);
        if (PtrInt(Dest)<DestLen) and (PtrInt(Source)<SourceLen) then continue else break;
      end else
      if c>$7ff then begin
        Dest[0] := AnsiChar($E0 or (c shr 12));
        Dest[1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[2] := AnsiChar($80 or (c and $3F));
        inc(Dest,3);
        if (PtrInt(Dest)<DestLen) and (PtrInt(Source)<SourceLen) then continue else break;
      end else begin
        Dest[0] := AnsiChar($C0 or (c shr 6));
        Dest[1] := AnsiChar($80 or (c and $3F));
        inc(Dest,2);
        if (PtrInt(Dest)<DestLen) and (PtrInt(Source)<SourceLen) then continue else break;
      end;
    until false;
    Dest^ := #0;
  end;
  result := PtrInt(Dest)-result;
end;

/// convert a RawUnicode PChar into a UTF-8 string
function RawUnicodeToUtf8(P: PWideChar; WideCharCount: integer): RawUTF8;
var L,LW: integer;
    U8: array[0..511] of AnsiChar;
begin
  if WideCharCount=0 then begin
    result := '';
    exit;
  end;
  LW := WideCharCount*3; // maximum resulting length
  if LW<SizeOf(U8) then begin // faster computation without temporary heap allocation
    SetString(Result,U8,RawUnicodeToUtf8(U8,sizeof(U8),P,WideCharCount));
    exit;
  end;
  SetString(result,nil,LW);
  L := RawUnicodeToUtf8(pointer(result),LW+1,P,WideCharCount);
  if L<=0 then
    result := '' else
    if L<>LW then
      SetLength(result,L);
end;

function RawUnicodeToUtf8(P: PWideChar; WideCharCount: integer; out UTF8Length: integer): RawUTF8; overload;
var LW: integer;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if WideCharCount=0 then
    exit;
  LW := WideCharCount*3; // maximum resulting length
  SetLength(result,LW);
  UTF8Length := RawUnicodeToUtf8(pointer(result),LW+1,P,WideCharCount);
  if UTF8Length<=0 then
    result := '';
end;

/// convert a RawUnicode string into a UTF-8 string
function RawUnicodeToUtf8(const Unicode: RawUnicode): RawUTF8;
begin
  result := RawUnicodeToUtf8(pointer(Unicode),length(Unicode) shr 1);
end;

function SynUnicodeToUtf8(const Unicode: SynUnicode): RawUTF8;
begin
  result := RawUnicodeToUtf8(pointer(Unicode),length(Unicode));
end;

function RawUnicodeToSynUnicode(const Unicode: RawUnicode): Synunicode;
begin
  SetString(result,PWideChar(pointer(Unicode)),length(Unicode) shr 1);
end;

function RawUnicodeToSynUnicode(P: PWideChar; WideCharCount: integer): SynUnicode; overload;
begin
  SetString(result,P,WideCharCount);
end;

procedure RawUnicodeToWinPChar(dest: PAnsiChar; source: PWideChar; WideCharCount: Integer);
begin
  WinAnsiConvert.UnicodeBufferToAnsi(dest,source,WideCharCount);
end;

function RawUnicodeToWinAnsi(P: PWideChar; WideCharCount: integer): WinAnsiString; overload;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(P,WideCharCount);
end;

function RawUnicodeToWinAnsi(const Unicode: RawUnicode): WinAnsiString;
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(pointer(Unicode),length(Unicode) shr 1);
end;

function WideStringToWinAnsi(const Wide: WideString): WinAnsiString; 
begin
  result := WinAnsiConvert.UnicodeBufferToAnsi(pointer(Wide),length(Wide));
end;

procedure UnicodeBufferToWinAnsi(source: PWideChar; out Dest: WinAnsiString);
var L: integer;
begin
  L := StrLenW(source);
  SetLength(Dest,L);
  WinAnsiConvert.UnicodeBufferToAnsi(pointer(Dest),source,L);
end;

function UnicodeBufferToString(source: PWideChar): string;
begin
  result := RawUnicodeToString(source,StrLenW(source));
end;

procedure AnsiCharToUTF8(P: PAnsiChar; L: Integer; var result: RawUTF8; ACP: integer);
begin
  result := TSynAnsiConvert.Engine(ACP).AnsiBufferToRawUTF8(P,L);
end;

{$ifdef UNICODE}
function UnicodeStringToUtf8(const S: string): RawUTF8;
begin
  result := RawUnicodeToUtf8(pointer(S),length(S));
end;

function UnicodeStringToWinAnsi(const S: string): WinAnsiString;
begin
  result := RawUnicodeToWinAnsi(pointer(S),length(S));
end;

function UTF8DecodeToUnicodeString(const S: RawUTF8): UnicodeString; overload;
begin
  result := UTF8DecodeToUnicodeString(pointer(S),length(S));
end;

function UTF8DecodeToUnicodeString(P: PUTF8Char; L: integer): UnicodeString; overload;
var L2: integer;
begin
  result := ''; // somewhat faster if result is freed before any SetLength()
  if L=0 then
    L := StrLen(P);
  if L=0 then
    exit;
  SetString(result,nil,L); // maximum posible unicode size (if all <#128)
  L2 := UTF8ToWideChar(pointer(result),P,L) shr 1;
  if L2<>L then
    SetLength(result,L2);
end;

function WinAnsiToUnicodeString(WinAnsi: PAnsiChar; WinAnsiLen: integer): UnicodeString;
begin
  SetString(result,nil,WinAnsiLen);
  WinAnsiConvert.AnsiBufferToUnicode(pointer(result),WinAnsi,WinAnsiLen);
end;
{$endif}

{$ifdef UNICODE}
function Ansi7ToString(const Text: RawByteString): string;
var i: integer;
begin
  SetString(result,nil,length(Text));
  for i := 0 to length(Text)-1 do
    PWordArray(result)[i] := PByteArray(Text)[i]; // no conversion for 7 bit Ansi
end;
{$else}
function Ansi7ToString(const Text: RawByteString): string;
begin
  result := Text; // if we are SURE this text is 7 bit Ansi -> direct assign
end;
{$endif}

{$ifdef UNICODE}
function Ansi7ToString(Text: PWinAnsiChar; Len: integer): string;
var i: integer;
begin
  SetString(result,nil,Len);
  for i := 0 to Len-1 do
    PWordArray(result)[i] := PByteArray(Text)[i]; // no conversion for 7 bit Ansi
end;
{$else}
function Ansi7ToString(Text: PWinAnsiChar; Len: integer): string;
begin
  SetString(result,PAnsiChar(Text),Len);
end;
{$endif}

{$ifdef UNICODE}
function StringToAnsi7(const Text: string): RawByteString;
var i: integer;
begin
  SetString(result,nil,length(Text));
  for i := 0 to length(Text)-1 do
    PByteArray(result)[i] := PWordArray(Text)[i]; // no conversion for 7 bit Ansi
end;
{$else}
function StringToAnsi7(const Text: string): RawByteString;
begin
  result := Text; // if we are SURE this text is 7 bit Ansi -> direct assign
end;
{$endif}

{$ifdef UNICODE}
function StringToWinAnsi(const Text: string): WinAnsiString;
begin
  result := RawUnicodeToWinAnsi(Pointer(Text),length(Text));
end;
{$else}
function StringToWinAnsi(const Text: string): WinAnsiString;
begin
  result := WinAnsiConvert.AnsiToAnsi(CurrentAnsiConvert,Text);
end;
{$endif}

{$ifdef UNICODE}
function StringBufferToUtf8(Dest: PUTF8Char; Source: PChar; SourceChars: PtrInt): PUTF8Char;
begin
  result := Dest+RawUnicodeToUtf8(Dest,SourceChars*3,PWideChar(Source),SourceChars);
end;
{$else}
function StringBufferToUtf8(Dest: PUTF8Char; Source: PChar; SourceChars: PtrInt): PUTF8Char;
begin
  result := CurrentAnsiConvert.AnsiBufferToUTF8(Dest,Source,SourceChars);
end;
{$endif}

{$ifdef UNICODE}
function StringToUTF8(const Text: string): RawUTF8;
begin
  result := RawUnicodeToUtf8(pointer(Text),length(Text));
end;
{$else}
function StringToUTF8(const Text: string): RawUTF8;
begin
  result := CurrentAnsiConvert.AnsiToUTF8(Text);
end;
{$endif}

{$ifndef LVCL}
function VariantToUTF8(const V: Variant): RawUTF8;
var U: SynUnicode;
begin
  U := V;
  result := RawUnicodeToUtf8(pointer(U),length(U));
end;
{$endif}

{$ifdef UNICODE}
function StringToRawUnicode(const S: string): RawUnicode;
begin
  SetString(result,PAnsiChar(pointer(S)),length(S)*2+1); // +1 for last wide #0
end;
{$else}
function StringToRawUnicode(const S: string): RawUnicode;
begin
  result := CurrentAnsiConvert.AnsiToRawUnicode(S);
end;
{$endif}

{$ifdef UNICODE}
function StringToSynUnicode(const S: string): SynUnicode;
begin
  result := S;
end;
{$else}
function StringToSynUnicode(const S: string): SynUnicode;
begin
  result := CurrentAnsiConvert.AnsiToUnicodeString(pointer(S),length(S));
end;
{$endif}

{$ifdef UNICODE}
function StringToRawUnicode(P: PChar; L: integer): RawUnicode;
begin
  SetString(result,PAnsiChar(P),L*2+1); // +1 for last wide #0
end;
{$else}
function StringToRawUnicode(P: PChar; L: integer): RawUnicode;
begin
  result := CurrentAnsiConvert.AnsiToRawUnicode(P,L);
end;
{$endif}


{$ifdef UNICODE}
function RawUnicodeToString(P: PWideChar; L: integer): string; overload;
begin
  SetString(result,P,L);
end;
{$else}
function RawUnicodeToString(P: PWideChar; L: integer): string; overload;
begin
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(P,L);
end;
{$endif}

{$ifdef UNICODE}
procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string); overload;
begin
  SetString(result,P,L);
end;
{$else}
procedure RawUnicodeToString(P: PWideChar; L: integer; var result: string); overload;
begin
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(P,L);
end;
{$endif}

{$ifdef UNICODE}
function RawUnicodeToString(const U: RawUnicode): string;
begin // uses StrLenW() and not length(U) to handle case when was used as buffer
  SetString(result,PWideChar(pointer(U)),StrLenW(Pointer(U)));
end;
{$else}
function RawUnicodeToString(const U: RawUnicode): string;
begin // uses StrLenW() and not length(U) to handle case when was used as buffer
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(Pointer(U),StrLenW(Pointer(U)));
end;
{$endif}

{$ifdef UNICODE}
function SynUnicodeToString(const U: SynUnicode): string;
begin 
  result := U;
end;
{$else}
function SynUnicodeToString(const U: SynUnicode): string;
begin 
  result := CurrentAnsiConvert.UnicodeBufferToAnsi(Pointer(U),length(U));
end;
{$endif}

{$ifdef UNICODE}
function UTF8DecodeToString(P: PUTF8Char; L: integer): string;
begin
  result := UTF8DecodeToUnicodeString(P,L);
end;
{$else}
function UTF8DecodeToString(P: PUTF8Char; L: integer): string;
begin
  result := CurrentAnsiConvert.UTF8BufferToAnsi(P,L);
end;
{$endif}

{$ifdef UNICODE}
function UTF8ToString(const Text: RawUTF8): string;
begin
  result := UTF8DecodeToUnicodeString(pointer(Text),length(Text));
end;
{$else}
function UTF8ToString(const Text: RawUTF8): string;
begin
  result := CurrentAnsiConvert.UTF8BufferToAnsi(pointer(Text),length(Text));
end;
{$endif}

function UTF8ToWideString(const Text: RawUTF8): WideString;
begin
{$ifdef FPC}
  result := '';
{$endif}
  UTF8ToWideString(Text,result);
end;

procedure UTF8ToWideString(const Text: RawUTF8; var result: WideString);
begin
  UTF8ToWideString(pointer(Text),Length(Text),result);
end;

procedure UTF8ToWideString(Text: PUTF8Char; Len: integer; var result: WideString); overload;
var tmp: RawUnicode;
    short: array[byte] of WideChar;
begin
  if Len<sizeof(short)shr 1 then
    SetString(result,short,UTF8ToWideChar(short,pointer(Text),Len) shr 1) else begin
    SetLength(tmp,Len*2+1); // maximum posible unicode size (if all <#128)
    SetString(result,PWideChar(pointer(tmp)),
      UTF8ToWideChar(pointer(tmp),pointer(Text),Len) shr 1);
  end;
end;

function WideStringToUTF8(const aText: WideString): RawUTF8;
begin
  result := RawUnicodeToUtf8(pointer(aText),length(aText));
end;

function UTF8ToSynUnicode(const Text: RawUTF8): SynUnicode;
var tmp: RawUnicode;
    L: integer;
    short: array[byte] of WideChar;
begin
  L := length(Text);
  if L<sizeof(short)shr 1 then
    SetString(result,short,UTF8ToWideChar(short,pointer(Text),L) shr 1) else begin
    SetLength(tmp,L*2+1); // maximum posible unicode size (if all <#128)
    SetString(result,PWideChar(pointer(tmp)),
      UTF8ToWideChar(pointer(tmp),pointer(Text),L) shr 1);
  end;
end;

procedure UTF8ToSynUnicode(const Text: RawUTF8; var result: SynUnicode); overload;
var tmp: RawUnicode;
    L: integer;
    short: array[byte] of WideChar;
begin
  L := length(Text);
  if L<sizeof(short)shr 1 then
    SetString(result,short,UTF8ToWideChar(short,pointer(Text),L) shr 1) else begin
    SetLength(tmp,L*2+1); // maximum posible unicode size (if all <#128)
    SetString(result,PWideChar(pointer(tmp)),
      UTF8ToWideChar(pointer(tmp),pointer(Text),L) shr 1);
  end;
end;

function StrInt32(P: PAnsiChar; val: PtrInt): PAnsiChar;
var c,c10: PtrUInt;
begin // this code is faster than the Borland's original str() or IntToStr()
  if val<0 then
    c := -val else
    c := val;
  repeat
    if c<10 then begin
      dec(P);
      P^ := AnsiChar(c+ord('0'));
      break;
    end else
    if c<100 then begin
      dec(P,2);
      PWord(P)^ := TwoDigitLookupW[c];
      break;
    end;
    c10 := c div 100;   // one div by two digits
{$ifdef CPU64} // CPU64 has no LEA optimization for *10 but has one cycle mul
    dec(c,c10*100);     {$else}
    dec(c,(c10*10)*10); // fast c := c mod 100
{$endif}
    dec(P,2);
    PWord(P)^ := TwoDigitLookupW[c];
    c := c10;
    if c10=0 then break;
  until false;
  if val<0 then begin
    dec(P);
    P^ := '-';
  end;
  result := P;
end;

function StrUInt32(P: PAnsiChar; val: PtrUInt): PAnsiChar;
var c10: PtrUInt;
begin // this code is faster than the Borland's original str() or IntToStr()
  repeat
    if val<10 then begin
      dec(P);
      P^ := AnsiChar(val+ord('0'));
      break;
    end else
    if val<100 then begin
      dec(P,2);
      PWord(P)^ := TwoDigitLookupW[val];
      break;
    end;
    c10 := val div 100;   // one div by two digits
{$ifdef CPU64} // CPU64 has one cycle mul
    dec(val,c10*100);     {$else}
    dec(val,(c10*10)*10); // fast val := val mod 100
{$endif}
    dec(P,2);
    PWord(P)^ := TwoDigitLookupW[val];
    val := c10;
    if c10=0 then break;
  until false;
  result := P;
end;

{$ifdef CPU64}
function StrInt64(P: PAnsiChar; val: Int64): PAnsiChar;
begin // StrInt32 aldready implemented PtrInt=Int64
  result := StrInt32(P,val);
end;
{$else}
function StrInt64(P: PAnsiChar; val: Int64): PAnsiChar;
var c,c10: Int64;
    c64: Int64Rec absolute c;
begin // this code is faster than the Borland's original str() or IntToStr()
  if val<0 then
    c := -val else
    c := val;
  repeat
    if c64.Hi=0 then begin
      P := StrUInt32(P,c64.Lo);
      break;
    end;
    c10 := c div 100;   // one div by two digits
    dec(c,c10*100);     // fast c := c mod 100
    dec(P,2);
    PWord(P)^ := TwoDigitLookupW[c];
    c := c10;
    if c10=0 then break;
  until false;
  if val<0 then begin
    dec(P);
    P^ := '-';
  end;
  result := P;
end;
{$endif}



type
  PStrRec = ^TStrRec;
  /// map the Delphi string header, as defined in System.pas
  TStrRec = packed record
{$ifdef UNICODE}
    /// the associated code page used for this string
    // - exist only since Delphi 2009
    // - 0 or 65535 for RawByteString
    // - 1200=CP_UTF16 for UnicodeString
    // - 65001=CP_UTF8 for RawUTF8
    // - the current code page for AnsiString
    codePage: Word;
    /// either 1 (for AnsiString) or 2 (for UnicodeString)
    // - exist only since Delphi 2009
    elemSize: Word;
{$endif}
    /// string reference count (basic garbage memory mechanism)
    refCnt: Longint;
    /// length in characters
    // - size in bytes = length*elemSize
    length: Longint;
  end;

  /// map the Delphi dynamic array header
  TDynArrayRec = packed record
    /// dynamic array reference count (basic garbage memory mechanism)
    refCnt: Longint;
    {$ifdef FPC}
    high: sizeint;
    function length: sizeint; inline;
    {$else}
    /// length in element count
    // - size in bytes = length*ElemSize
    length: Longint;
    {$endif}
  end;

const
  /// codePage offset = string header size
  // - used to calc the beginning of memory allocation of a string
  STRRECSIZE = SizeOf(TStrRec);

{$ifdef FPC}
function TDynArrayRec.length: sizeint;
begin
  result := high+1;
end;
{$endif}

{$ifdef UNICODE}
// this Pos() is seldom used, it was decided to only define it under
// Delphi 2009+ (which expect such a RawUTF8 specific overloaded version)

function Pos(const substr, str: RawUTF8): Integer; overload;
begin
  Result := PosEx(substr, str, 1);
end;

function IntToString(Value: integer): string;
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt32(@tmp[15],Value);
  result := Ansi7ToString(PWinAnsiChar(P),@tmp[15]-P);
end;

function IntToString(Value: cardinal): string;
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrUInt32(@tmp[15],Value);
  result := Ansi7ToString(PWinAnsiChar(P),@tmp[15]-P);
end;

function IntToString(Value: Int64): string;
var tmp: array[0..31] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt64(@tmp[31],Value);
  result := Ansi7ToString(PWinAnsiChar(P),@tmp[31]-P);
end;

function DoubleToString(Value: Double): string;
var tmp: ShortString;
begin
  if Value=0 then
    result := '0' else
    result := Ansi7ToString(PWinAnsiChar(@tmp[1]),
      ExtendedToString(tmp,Value,DOUBLE_PRECISION));
end;

function Curr64ToString(Value: Int64): string;
var tmp: array[0..31] of AnsiChar;
begin
  result := Ansi7ToString(tmp,Curr64ToPChar(Value,tmp));
end;

{$else UNICODE}

function IntToString(Value: integer): string;
{$ifdef PUREPASCAL}
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt32(@tmp[15],Value);
  SetString(result,P,@tmp[15]-P);
end;
{$else}
asm
  jmp Int32ToUTF8
end;
{$endif}

function IntToString(Value: cardinal): string;
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrUInt32(@tmp[15],Value);
  SetString(result,P,@tmp[15]-P);
end;

function IntToString(Value: Int64): string;
var tmp: array[0..31] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt64(@tmp[31],Value);
  SetString(result,P,@tmp[31]-P);
end;

function DoubleToString(Value: Double): string;
var tmp: ShortString;
begin
  if Value=0 then
    result := '0' else
    SetString(result,PAnsiChar(@tmp[1]),ExtendedToString(tmp,Value,DOUBLE_PRECISION));
end;

function Curr64ToString(Value: Int64): string;
begin
  result := Curr64ToStr(Value);
end;

{$endif UNICODE}

{$ifndef FPC}
procedure FillCharInvoke;
asm
  call System.@FillChar
end;
{$endif}

{$ifndef ENHANCEDRTL} { our Enhanced Runtime (or LVCL) library contain fast asm versions }

{ code below was extracted from our Enhanced Runtime (or LVCL) library
   and increases the framework performance
  - not compiled with FPC, since does call some low-level system.pas functions  }

{$ifndef PUREPASCAL} { these functions are implemented in asm }
{$ifndef LVCL} { don't define these functions twice }
{$ifndef FPC}  { these asm function use some low-level system.pas calls }

{$define OWNI2S}

function Int32ToUTF8(Value : integer): RawUTF8; // 3x faster than SysUtils.IntToStr
// from IntToStr32_JOH_IA32_6_a, adapted for Delphi 2009+
asm // eax=Value, edx=@result
  push   ebx
  push   edi
  push   esi
  mov    ebx,eax                {Value}
  sar    ebx,31                 {0 for +ve Value or -1 for -ve Value}
  xor    eax,ebx
  sub    eax,ebx                {ABS(Value)}
  mov    esi,10                 {Max Digits in result}
  mov    edi,edx                {@result}
  cmp    eax,10;         sbb    esi, 0
  cmp    eax,100;        sbb    esi, 0
  cmp    eax,1000;       sbb    esi, 0
  cmp    eax,10000;      sbb    esi, 0
  cmp    eax,100000;     sbb    esi, 0
  cmp    eax,1000000;    sbb    esi, 0
  cmp    eax,10000000;   sbb    esi, 0
  cmp    eax,100000000;  sbb    esi, 0
  cmp    eax,1000000000; sbb    esi, ebx    {esi=Digits (Including Sign Character)}
  mov    ecx,[edx]              {result}
  test   ecx,ecx
  je     @@NewStr               {Create New string for result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr            {Reference Count <> 1}
  cmp    esi,[ecx-4]
  je     @@LengthOk             {Existing Length = Required Length}
  sub    ecx,STRRECSIZE         {Allocation Address}
  push   eax                    {ABS(Value)}
  push   ecx
  mov    eax,esp
  lea    edx,[esi+STRRECSIZE+1] {New Allocation Size}
  call   system.@ReallocMem     {Reallocate result string}
  pop    ecx
  pop    eax                    {ABS(Value)}
  add    ecx,STRRECSIZE         {result}
  mov    [ecx-4],esi            {Set New Length}
  mov    byte ptr [ecx+esi],0   {Add Null Terminator}
  mov    [edi],ecx              {Set result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx,dword ptr [ecx-8]  {Reference Count}
  add     edx,1
  jz      @@NewStr               {RefCount = -1 (string Constant)}
  lock    dec dword ptr [ecx-8]  {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax,esi                 {Length}
{$ifdef UNICODE}
  mov    edx,CP_UTF8 // UTF-8 code page for Delphi 2009+
{$endif}
  call   system.@NewAnsiString
  mov    [edi],eax               {Set result Address}
  mov    ecx,eax                 {result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    byte ptr [ecx],'-'      {Store '-' Character (May be Overwritten)}
  add    esi,ebx                 {Digits (Excluding Sign Character)}
  sub    ecx,ebx                 {Destination of 1st Digit}
  sub    esi,2                   {Digits (Excluding Sign Character) - 2}
  jle    @@FinalDigits           {1 or 2 Digit Value}
  cmp    esi,8                   {10 Digit Value?}
  jne    @@SetResult             {Not a 10 Digit Value}
  sub    eax,2000000000          {Digit 10 must be either '1' or '2'}
  mov    dl,'2'
  jnc    @@SetDigit10            {Digit 10 = '2'}
  mov    dl,'1'                  {Digit 10 = '1'}
  add    eax,1000000000
@@SetDigit10:
  mov    [ecx],dl                {Save Digit 10}
  mov    esi,7                   {9 Digits Remaining}
  add    ecx,1                   {Destination of 2nd Digit}
@@SetResult:
  mov    edi,$28F5C29            {((2^32)+100-1)/100}
@@Loop:
  mov    ebx,eax                 {Dividend}
  mul    edi                     {EDX = Dividend DIV 100}
  mov    eax,edx                 {Set Next Dividend}
  imul   edx,-200                {-2 * (100 * Dividend DIV  100)}
  movzx  edx,word ptr [TwoDigitLookup+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [ecx+esi],dx
  sub    esi,2
  jg     @@Loop                  {Loop until 1 or 2 Digits Remaining}
@@FinalDigits:
  pop    esi
  pop    edi
  pop    ebx
  jnz    @@LastDigit
  movzx  eax,word ptr [TwoDigitLookup+eax*2]
  mov    [ecx],ax                {Save Final 2 Digits}
  ret
@@LastDigit:
  or     al,'0'                  {Ascii Adjustment}
  mov    [ecx],al                {Save Final Digit}
end;

function Int64ToUTF8(Value: Int64): RawUTF8;
// from IntToStr64_JOH_IA32_6_b, adapted for Delphi 2009+ 
asm
  push   ebx
  mov    ecx, [ebp+8]            {Low Integer of Value}
  mov    edx, [ebp+12]           {High Integer of Value}
  xor    ebp, ebp                {Clear Sign Flag (EBP Already Pushed)}
  mov    ebx, ecx                {Low Integer of Value}
  test   edx, edx
  jnl    @@AbsValue
  mov    ebp, 1                  {EBP = 1 for -ve Value or 0 for +ve Value}
  neg    ecx
  adc    edx, 0
  neg    edx
@@AbsValue:                      {EDX:ECX = Abs(Value)}
  jnz    @@Large
  test   ecx, ecx
  js     @@Large
  mov    edx, eax                {@Result}
  mov    eax, ebx                {Low Integer of Value}
  call   Int32ToUTF8               {Call Fastest Integer IntToStr Function}
  pop    ebx
@@Exit:
  pop    ebp                     {Restore Stack and Exit}
  ret    8
@@Large:
  push   edi
  push   esi
  mov    edi, eax
  xor    ebx, ebx
  xor    eax, eax
@@Test15:                        {Test for 15 or More Digits}
  cmp    edx, $00005af3          {100000000000000 div $100000000}
  jne    @@Check15
  cmp    ecx, $107a4000          {100000000000000 mod $100000000}
@@Check15:
  jb     @@Test13
@@Test17:                        {Test for 17 or More Digits}
  cmp    edx, $002386f2          {10000000000000000 div $100000000}
  jne    @@Check17
  cmp    ecx, $6fc10000          {10000000000000000 mod $100000000}
@@Check17:
  jb     @@Test15or16
@@Test19:                        {Test for 19 Digits}
  cmp    edx, $0de0b6b3          {1000000000000000000 div $100000000}
  jne    @@Check19
  cmp    ecx, $a7640000          {1000000000000000000 mod $100000000}
@@Check19:
  jb     @@Test17or18
  mov    al, 19
  jmp    @@SetLength
@@Test17or18:                    {17 or 18 Digits}
  mov    bl, 18
  cmp    edx, $01634578          {100000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $5d8a0000          {100000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test15or16:                    {15 or 16 Digits}
  mov    bl, 16
  cmp    edx, $00038d7e          {1000000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $a4c68000          {1000000000000000 mod $100000000}
  jmp    @@SetLen
@@Test13:                        {Test for 13 or More Digits}
  cmp    edx, $000000e8          {1000000000000 div $100000000}
  jne    @@Check13
  cmp    ecx, $d4a51000          {1000000000000 mod $100000000}
@@Check13:
  jb     @@Test11
@@Test13or14:                    {13 or 14 Digits}
  mov    bl, 14
  cmp    edx, $00000918          {10000000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4e72a000          {10000000000000 mod $100000000}
  jmp    @@SetLen
@@Test11:                        {10, 11 or 12 Digits}
  cmp    edx, $02                {10000000000 div $100000000}
  jne    @@Check11
  cmp    ecx, $540be400          {10000000000 mod $100000000}
@@Check11:
  mov    bl, 11
  jb     @@SetLen                {10 Digits}
@@Test11or12:                    {11 or 12 Digits}
  mov    bl, 12
  cmp    edx, $17                {100000000000 div $100000000}
  jne    @@SetLen
  cmp    ecx, $4876e800          {100000000000 mod $100000000}
@@SetLen:
  sbb    eax, 0                  {Adjust for Odd/Evem Digit Count}
  add    eax, ebx
@@SetLength:                     {Abs(Value) in EDX:ECX, Digits in EAX}
  push   ecx                     {Save Abs(Value)}
  push   edx
  lea    edx, [eax+ebp]          {Digits Needed (Including Sign Character)}
  mov    ecx, [edi]              {@Result}
  mov    esi, edx                {Digits Needed (Including Sign Character)}
  test   ecx, ecx
  je     @@NewStr                {Create New AnsiString for Result}
  cmp    dword ptr [ecx-8], 1
  jne    @@ChangeStr             {Reference Count <> 1}
  cmp    esi, [ecx-4]
  je     @@LengthOk              {Existing Length = Required Length}
  sub    ecx, STRRECSIZE         {Allocation Address}
  push   eax                     {ABS(Value)}
  push   ecx
  mov    eax, esp
  lea    edx, [esi+STRRECSIZE+1] {New Allocation Size}
  call   system.@ReallocMem      {Reallocate Result AnsiString}
  pop    ecx
  pop    eax                     {ABS(Value)}
  add    ecx, STRRECSIZE         {@Result}
  mov    [ecx-4], esi            {Set New Length}
  mov    byte ptr [ecx+esi], 0   {Add Null Terminator}
  mov    [edi], ecx              {Set Result Address}
  jmp    @@LengthOk
@@ChangeStr:
  mov     edx, dword ptr [ecx-8]  {Reference Count}
  add     edx, 1
  jz      @@NewStr                {RefCount = -1 (AnsiString Constant)}
  lock    dec dword ptr [ecx-8]   {Decrement Existing Reference Count}
@@NewStr:
  push   eax                     {ABS(Value)}
  mov    eax, esi                {Length}
{$ifdef UNICODE}
  mov    edx,CP_UTF8 // UTF-8 code page for Delphi 2009+
{$endif}
  call   system.@NewAnsiString
  mov    [edi], eax              {Set Result Address}
  mov    ecx, eax                {@Result}
  pop    eax                     {ABS(Value)}
@@LengthOk:
  mov    edi, [edi]              {@Result}
  sub    esi, ebp                {Digits Needed (Excluding Sign Character)}
  mov    byte ptr [edi], '-'     {Store '-' Character (May be Overwritten)}
  add    edi, ebp                {Destination of 1st Digit}
  pop    edx                     {Restore Abs(Value)}
  pop    eax
  cmp    esi, 17
  jl     @@LessThan17Digits      {Digits < 17}
  je     @@SetDigit17            {Digits = 17}
  cmp    esi, 18
  je     @@SetDigit18            {Digits = 18}
  mov    cl, '0' - 1
  mov    ebx, $a7640000          {1000000000000000000 mod $100000000}
  mov    ebp, $0de0b6b3          {1000000000000000000 div $100000000}
@@CalcDigit19:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit19
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit18:
  mov    cl, '0' - 1
  mov    ebx, $5d8a0000          {100000000000000000 mod $100000000}
  mov    ebp, $01634578          {100000000000000000 div $100000000}
@@CalcDigit18:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit18
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1
@@SetDigit17:
  mov    cl, '0' - 1
  mov    ebx, $6fc10000          {10000000000000000 mod $100000000}
  mov    ebp, $002386f2          {10000000000000000 div $100000000}
@@CalcDigit17:
  add    ecx, 1
  sub    eax, ebx
  sbb    edx, ebp
  jnc    @@CalcDigit17
  add    eax, ebx
  adc    edx, ebp
  mov    [edi], cl
  add    edi, 1                  {Update Destination}
  mov    esi, 16                 {Set 16 Digits Left}
@@LessThan17Digits:              {Process Next 8 Digits}
  mov    ecx, 100000000          {EDX:EAX = Abs(Value) = Dividend}
  div    ecx
  mov    ebp, eax                {Dividend DIV 100000000}
  mov    ebx, edx
  mov    eax, edx                {Dividend MOD 100000000}
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx                {Set Next Dividend}
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ebx, edx                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookup+ebx*2]
  shl    ebx, 16
  mov    edx, $51EB851F
  mov    ecx, eax                {Dividend}
  mul    edx
  shr    edx, 5                  {Dividend DIV 100}
  mov    eax, edx
  lea    edx, [edx*4+edx]
  lea    edx, [edx*4+edx]
  shl    edx, 2                  {Dividend DIV 100 * 100}
  sub    ecx, edx                {Remainder (0..99)}
  or     bx, word ptr [TwoDigitLookup+ecx*2]
  mov    [edi+esi-4], ebx        {Store 4 Digits}
  mov    ebx, eax
  mov    edx, $51EB851F
  mul    edx
  shr    edx, 5                  {EDX = Dividend DIV 100}
  lea    eax, [edx*4+edx]
  lea    eax, [eax*4+eax]
  shl    eax, 2                  {EAX = Dividend DIV 100 * 100}
  sub    ebx, eax                {Remainder (0..99)}
  movzx  ebx, word ptr [TwoDigitLookup+ebx*2]
  movzx  ecx, word ptr [TwoDigitLookup+edx*2]
  shl    ebx, 16
  or     ebx, ecx
  mov    [edi+esi-8], ebx        {Store 4 Digits}
  mov    eax, ebp                {Remainder}
  sub    esi, 10                 {Digits Left - 2}
  jz     @@Last2Digits
@@SmallLoop:                     {Process Remaining Digits}
  mov    edx, $28F5C29           {((2^32)+100-1)/100}
  mov    ebx, eax                {Dividend}
  mul    edx
  mov    eax, edx                {Set Next Dividend}
  imul   edx, -200
  movzx  edx, word ptr [TwoDigitLookup+ebx*2+edx] {Dividend MOD 100 in ASCII}
  mov    [edi+esi], dx
  sub    esi, 2
  jg     @@SmallLoop             {Repeat Until Less than 2 Digits Remaining}
  jz     @@Last2Digits
  or     al , '0'                {Ascii Adjustment}
  mov    [edi], al               {Save Final Digit}
  jmp    @@Done
@@Last2Digits:
  movzx  eax, word ptr [TwoDigitLookup+eax*2]
  mov    [edi], ax               {Save Final 2 Digits}
@@Done:
  pop    esi
  pop    edi
  pop    ebx
end;

function Trim(const S: RawUTF8): RawUTF8;
asm  // fast implementation by John O'Harrow, modified for Delphi 2009+
  test eax,eax                   {S = nil?}
  xchg eax,edx
  jz   System.@LStrClr           {Yes, Return Empty String}
  mov  ecx,[edx-4]               {Length(S)}
  cmp  byte ptr [edx],' '        {S[1] <= ' '?}
  jbe  @@TrimLeft                {Yes, Trim Leading Spaces}
  cmp  byte ptr [edx+ecx-1],' '  {S[Length(S)] <= ' '?}
  jbe  @@TrimRight               {Yes, Trim Trailing Spaces}
  jmp  System.@LStrLAsg          {No, Result := S (which occurs most time)}
@@TrimLeft:                      {Strip Leading Whitespace}
  dec  ecx
  jle  System.@LStrClr           {All Whitespace}
  inc  edx
  cmp  byte ptr [edx],' '
  jbe  @@TrimLeft
@@CheckDone:
  cmp  byte ptr [edx+ecx-1],' '
{$ifdef UNICODE}
  jbe  @@TrimRight
  push CP_UTF8 // UTF-8 code page for Delphi 2009+
  call  System.@LStrFromPCharLen // we need a call, not a jmp here
  ret
{$else}
  ja   System.@LStrFromPCharLen
{$endif}
@@TrimRight:                     {Strip Trailing Whitespace}
  dec  ecx
  jmp  @@CheckDone
end;

function CompareMem(P1, P2: Pointer; Length: Integer): Boolean;
asm // eax=P1 edx=P2 ecx=Length
    cmp eax,edx
    push  ebx
    je    @@True                { P1=P2 }
    sub   ecx, 8
    jl    @@Small
    mov   ebx, [eax]         {Compare First 4 Bytes}
    cmp   ebx, [edx]
    jne   @@False
    lea   ebx, [eax+ecx]     {Compare Last 8 Bytes}
    add   edx, ecx
    mov   eax, [ebx]
    cmp   eax, [edx]
    jne   @@False
    mov   eax, [ebx+4]
    cmp   eax, [edx+4]
    jne   @@False
    sub   ecx, 4
    jle   @@True             {All Bytes already Compared}
    neg   ecx                {-(Length-12)}
    add   ecx, ebx           {DWORD Align Reads}
    and   ecx, -4
    sub   ecx, ebx
@@LargeLoop:               {Compare 8 Bytes per Loop}
    mov   eax, [ebx+ecx]
    cmp   eax, [edx+ecx]
    jne   @@False
    mov   eax, [ebx+ecx+4]
    cmp   eax, [edx+ecx+4]
    jne   @@False
    add   ecx, 8
    jl    @@LargeLoop
@@True:
    mov   al, 1
    pop   ebx
    ret
@Table:
    dd @@true, @1, @2, @3, @4, @5, @6, @7
@@Small: // ecx=0..7
    add   ecx, 8
    jle   @@True             {Length <= 0}
    jmp dword ptr [ecx*4+@Table]
@7: mov bl,[eax+6]; cmp bl,[edx+6]; jne @@False
@6: mov bh,[eax+5]; cmp bh,[edx+5]; jne @@False
@5: mov cl,[eax+4]; cmp cl,[edx+4]; jne @@False
@4: mov ch,[eax+3]; cmp ch,[edx+3]; jne @@False
@3: mov bl,[eax+2]; cmp bl,[edx+2]; jne @@False
@2: mov bh,[eax+1]; cmp bh,[edx+1]; jne @@False
@1: mov al,[eax];   cmp al,[edx];   je @@True
@@False:
  xor   eax, eax
  pop   ebx
end;
{$endif FPC}  { these asm function had some low-level system.pas calls }


{$ifndef DELPHI5OROLDER}
const
  FILLCHAR_SIZE = 160;

procedure FillChar(var Dest; Count: Integer; Value: Byte);
asm
  cmp       edx, 32
  mov       ch,cl                {copy value into both bytes of cx}
  jl        @@small
  sub       edx,16
  movd      xmm0,ecx
  pshuflw   xmm0,xmm0,0
  pshufd    xmm0,xmm0,0
  movups    [eax],xmm0           {fill first 16 bytes}
  movups    [eax+edx],xmm0       {fill last 16 bytes}
  mov       ecx,eax              {16-byte align writes}
  and       ecx,15
  sub       ecx,16
  sub       eax,ecx
  add       edx,ecx
  add       eax,edx
  neg       edx
  cmp       edx,-512*1024
  jb        @@large
@@loop:
  movaps    [eax+edx],xmm0       {fill 16 bytes per loop}
  add       edx,16
  jl        @@loop
  ret
@@large:
  movntdq    [eax+edx],xmm0      {fill 16 bytes per loop}
  add       edx,16
  jl        @@large
  ret
@@small:
  test      edx,edx
  jle       @@done
  mov       [eax+edx-1],cl       {fill last byte}
  and       edx,-2               {no. of words to fill}
  neg       edx
  lea       edx,[@@smallfill+60+edx*2]
  jmp       edx
  nop                             {align jump destinations}
  nop
@@smallfill:
  mov       [eax+28],cx
  mov       [eax+26],cx
  mov       [eax+24],cx
  mov       [eax+22],cx
  mov       [eax+20],cx
  mov       [eax+18],cx
  mov       [eax+16],cx
  mov       [eax+14],cx
  mov       [eax+12],cx
  mov       [eax+10],cx
  mov       [eax+ 8],cx
  mov       [eax+ 6],cx
  mov       [eax+ 4],cx
  mov       [eax+ 2],cx
  mov       [eax   ],cx
  ret {do not remove - this is for alignment}
@@done:
end;
{$endif DELPHI5OROLDER}


{$ifndef ISDELPHI2007ANDUP}
procedure MoveInvoke;
asm
  call System.Move
end;

/// faster implementation of Move() for Delphi versions with no FastCode inside
procedure Move(const Source; var Dest; Count: Integer);
asm // eax=source edx=dest ecx=count
         // original code by John O'Harrow - included since delphi 2007
        cmp     ecx,32
        ja      @lrg                  // count > 32 or count < 0
        sub     ecx,8
        jg      @sml                  // 9..32 byte move
        jmp     dword ptr [@table+32+ecx*4]   // 0..8 byte move
@sml:   fild    qword ptr [eax+ecx]   // load last 8
        fild    qword ptr [eax]       // load first 8
        cmp     ecx,8
        jle     @sml16
        fild    qword ptr [eax+8]     // load second 8
        cmp     ecx,16
        jle     @sml24
        fild    qword ptr [eax+16]    // load third 8
        fistp   qword ptr [edx+16]    // save third 8
@sml24: fistp   qword ptr [edx+8]     // save second 8
@sml16: fistp   qword ptr [edx]       // save first 8
        fistp   qword ptr [edx+ecx]   // save last 8
@exit:  ret
        lea eax,[eax+0]  // for 4-byte alignment of @table
@table: dd @exit,@m01,@m02,@m03,@m04,@m05,@m06,@m07,@m08
@lrgfwd:push    edx
        fild    qword ptr [eax]       // first 8
        lea     eax,[eax+ecx-8]
        lea     ecx,[ecx+edx-8]
        fild    qword ptr [eax]       // last 8
        push    ecx
        neg     ecx
        and     edx,-8                // 8-byte align writes
        lea     ecx,[ecx+edx+8]
        pop     edx
@fwd:   fild    qword ptr [eax+ecx]
        fistp   qword ptr [edx+ecx]
        add     ecx,8
        jl      @fwd
        fistp   qword ptr [edx]       // last 8
        pop     edx
        fistp   qword ptr [edx]       // first 8
        ret
@lrg:   jng     @done                 // count < 0
        cmp     eax,edx
        ja      @lrgfwd
        sub     edx,ecx
        cmp     eax,edx
        lea     edx,[edx+ecx]
        jna     @lrgfwd
        sub     ecx,8                 // backward move
        push    ecx
        fild    qword ptr [eax+ecx]   // last 8
        fild    qword ptr [eax]       // first 8
        add     ecx,edx
        and     ecx,-8                // 8-byte align writes
        sub     ecx,edx
@bwd:   fild    qword ptr [eax+ecx]
        fistp   qword ptr [edx+ecx]
        sub     ecx,8
        jg      @bwd
        pop     ecx
        fistp   qword ptr [edx]       // first 8
        fistp   qword ptr [edx+ecx]   // last 8
@done:  ret
@m01:   movzx   ecx,byte ptr [eax]
        mov     [edx],cl
        ret
@m02:   movzx   ecx,word ptr [eax]
        mov     [edx],cx
        ret
@m03:   mov     cx,[eax]
        mov     al,[eax+2]
        mov     [edx],cx
        mov     [edx+2],al
        ret
@m04:   mov     ecx,[eax]
        mov     [edx],ecx
        ret
@m05:   mov     ecx,[eax]
        mov     al,[eax+4]
        mov     [edx],ecx
        mov     [edx+4],al
        ret
@m06:   mov     ecx,[eax]
        mov     ax,[eax+4]
        mov     [edx],ecx
        mov     [edx+4],ax
        ret
@m07:   mov     ecx,[eax]
        mov     eax,[eax+3]
        mov     [edx],ecx
        mov     [edx+3],eax
        ret
@m08:   mov     ecx,[eax]
        mov     eax,[eax+4]
        mov     [edx],ecx
        mov     [edx+4],eax
end;

{$endif ISDELPHI2007ANDUP}

{$ifdef DELPHI5OROLDER}
procedure FillChar(var Dest; Count: Integer; Value: Byte);
{$else}
procedure FillCharX87(var Dest; Count: Integer; Value: Byte);
{$endif}
asm // eax=Dest edx=Count cl=Value
        // faster version by John O'Harrow  (Code Size = 153 Bytes)
        cmp   edx,32
        mov   ch,cl                 // copy value into both bytes of cx
        jl    @small
        mov   [eax  ],cx            // fill first 8 bytes
        mov   [eax+2],cx
        mov   [eax+4],cx
        mov   [eax+6],cx
        sub   edx,16
        fld   qword ptr [eax]
        fst   qword ptr [eax+edx]    // fill last 16 bytes
        fst   qword ptr [eax+edx+8]
        mov   ecx,eax
        and   ecx,7                 // 8-byte align writes
        sub   ecx,8
        sub   eax,ecx
        add   edx,ecx
        add   eax,edx
        neg   edx
@loop:  fst   qword ptr [eax+edx]    // fill 16 bytes per loop
        fst   qword ptr [eax+edx+8]
        add   edx,16
        jl    @loop
        ffree st(0)
        fincstp
        ret
        nop
@small: test  edx,edx
        jle   @done
        mov   [eax+edx-1],cl        // fill last byte
        and   edx,-2                // no. of words to fill
        neg   edx
        lea   edx,[@fill+60+edx*2]
        jmp   edx
        nop                          // align jump destinations
        nop
@fill:  mov   [eax+28],cx
        mov   [eax+26],cx
        mov   [eax+24],cx
        mov   [eax+22],cx
        mov   [eax+20],cx
        mov   [eax+18],cx
        mov   [eax+16],cx
        mov   [eax+14],cx
        mov   [eax+12],cx
        mov   [eax+10],cx
        mov   [eax+ 8],cx
        mov   [eax+ 6],cx
        mov   [eax+ 4],cx
        mov   [eax+ 2],cx
        mov   [eax   ],cx
        ret                         // for alignment
@done:  db $f3 // rep ret AMD trick here
end;

{$endif LVCL}
{$endif PUREPASCAL}
{$endif ENHANCEDRTL}

{$ifdef PUREPASCAL}
function PosEx(const SubStr, S: RawUTF8; Offset: Cardinal = 1): Integer;
var
  I,X: Integer;
  Len, LenSubStr: Integer;
  C: AnsiChar;
begin
  if Offset = 1 then
    Result := Pos(RawByteString(SubStr),RawByteString(S)) else begin
    I := Offset;
    LenSubStr := Length(SubStr);
    Len := Length(S)-LenSubStr+1;
    C := SubStr[1];
    while I<=Len do begin
      if S[I]=C then begin
        X := 1;
        while (X<LenSubStr) and (S[I+X]=SubStr[X+1]) do
          Inc(X);
        if X=LenSubStr then begin
          Result := I;
          exit;
        end;
      end;
      Inc(I);
    end;
    Result := 0;
  end;
end;
{$else}
function PosEx(const SubStr, S: RawUTF8; Offset: Cardinal = 1): Integer;
asm
  push    ebx
  push    esi
  push    edx              {@Str}
  test    eax, eax
  jz      @@NotFound       {Exit if SubStr = ''}
  test    edx, edx
  jz      @@NotFound       {Exit if Str = ''}
  mov     esi, ecx
  mov     ecx,[edx-4]     {Length(Str)}
  mov     ebx,[eax-4]     {Length(SubStr)}
  add     ecx, edx
  sub     ecx, ebx        {Max Start Pos for Full Match}
  lea     edx,[edx+esi-1] {Set Start Position}
  cmp     edx, ecx
  jg      @@NotFound       {StartPos > Max Start Pos}
  cmp     ebx, 1           {Length(SubStr)}
  jle     @@SingleChar     {Length(SubStr) <= 1}
  push    edi
  push    ebp
  lea     edi,[ebx-2]     {Length(SubStr) - 2}
  mov     esi, eax
  movzx   ebx,byte ptr [eax]       {Search Character}
@@Loop:                   {Compare 2 Characters per Loop}
  cmp     bl,[edx]
  jne     @@NotChar1
  mov     ebp, edi         {Remainder}
@@Char1Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp]
  jne     @@NotChar1
  sub     ebp, 2
  jnc     @@Char1Loop
  pop     ebp
  pop     edi
  jmp     @@SetResult
@@NotChar1:
  cmp     bl,[edx+1]
  jne     @@NotChar2
  mov     ebp, edi         {Remainder}
@@Char2Loop:
  movzx   eax, word ptr [esi+ebp]
  cmp     ax,[edx+ebp+1]
  jne     @@NotChar2
  sub     ebp, 2
  jnc     @@Char2Loop
  pop     ebp
  pop     edi
  jmp     @@CheckResult
@@NotChar2:
  lea     edx,[edx+2]
  cmp     edx, ecx         {Next Start Position <= Max Start Position}
  jle     @@Loop
  pop     ebp
  pop     edi
  jmp     @@NotFound
@@SingleChar:
  jl      @@NotFound       {Needed for Zero-Length Non-NIL Strings}
  movzx   eax,byte ptr [eax]       {Search Character}
@@CharLoop:
  cmp     al,[edx]
  je      @@SetResult
  cmp     al,[edx+1]
  je      @@CheckResult
  lea     edx,[edx+2]
  cmp     edx, ecx
  jle     @@CharLoop
@@NotFound:
  xor     eax, eax
  pop     edx
  pop     esi
  pop     ebx
  ret
@@CheckResult:             {Check within AnsiString}
  cmp     edx, ecx
  jge     @@NotFound
  add     edx, 1
@@SetResult:
  pop     ecx              {@Str}
  pop     esi
  pop     ebx
  neg     ecx
  lea     eax,[edx+ecx+1]
end;
{$endif PUREPASCAL}

procedure Split(const Str, SepStr: RawUTF8; var LeftStr, RightStr: RawUTF8; ToUpperCase: boolean);
var i: integer;
begin
  i := PosEx(SepStr,Str);
  if i=0 then begin
    LeftStr := Str;
    RightStr := '';
  end else begin
    LeftStr := copy(Str,1,i-1);
    RightStr := copy(Str,i+length(SepStr),maxInt);
  end;
  if ToUpperCase then begin
    LeftStr := UpperCaseU(LeftStr);
    RightStr := UpperCaseU(RightStr);
  end;
end;

function StringReplaceAll(const S, OldPattern, NewPattern: RawUTF8): RawUTF8;
procedure Process(j: integer);
var i: integer;
begin
  Result := '';
  i := 1;
  repeat
    Result := Result+Copy(S,i,j-i)+NewPattern;
    i := j+length(OldPattern);
    j := PosEx(OldPattern, S, i);
    if j=0 then begin
      Result := Result+Copy(S, i, maxInt);
      break;
    end;
  until false;
end;
var j: integer;
begin
  j := PosEx(OldPattern, S, 1); // our PosEx() is faster than Pos()
  if j=0 then
    result := S else
    Process(j);
end;

function PosChar(Str: PUTF8Char; Chr: AnsiChar): PUTF8Char;
{$ifdef PUREPASCAL}
begin
  Result := Str;
  while Result^<>Chr do begin
    if Result^=#0 then begin
      Result := nil;
      Exit;
    end;
    Inc(Result);
  end;
end;
{$else}
asm // faster version by AB - eax=Str dl=Chr
    or eax,eax
    jz @z
@1: mov cl,[eax]
    cmp cl,dl
    jz @z
    inc eax
    or cl,cl
    jnz @1
    xor eax,eax
@z:
end;
{$endif}

function StringReplaceChars(const Source: RawUTF8; OldChar, NewChar: AnsiChar): RawUTF8;
var c: AnsiChar;
    i: integer;
begin
  result := '';
  if Source='' then
    exit;
  SetLength(result,length(Source));
  i := 0;
  repeat
    c := PUTF8Char(pointer(Source))[i];
    if c=#0 then
      exit;
    if c=OldChar then
      c := NewChar;
    PUTF8Char(pointer(result))[i] := c;
    inc(i);
  until false;
end;

function PosI(substr: PUTF8Char; const str: RawUTF8): Integer;
var C: AnsiChar;
begin
  if substr<>nil then begin
    C := substr^;
    for result := 1 to Length(str) do
      if NormToUpperAnsi7[str[result]]=C then
        if IdemPChar(@PUTF8Char(pointer(str))[result],substr+1) then
          exit;
  end;
  result := 0;
end;

function PosIU(substr: PUTF8Char; const str: RawUTF8): Integer;
var p: PUTF8Char;
begin
  if (substr<>nil) and (str<>'') then begin
    p := pointer(str);
    repeat
      if GetNextUTF8Upper(p)=ord(substr^) then
        if IdemPCharU(p,substr+1) then begin
          result := p-pointer(str);
          exit;
        end;
    until p^=#0;
  end;
  result := 0;
end;

procedure AppendBufferToRawUTF8(var Text: RawUTF8; Buffer: pointer; BufferLen: PtrInt);
var L: PtrInt;
begin
  if BufferLen<=0 then
    exit;
  L := PtrInt(Text);
  if L<>0 then
    L := pInteger(L-4)^; // L := length(Text)
  SetLength(Text,L+BufferLen);
  move(Buffer^,pointer(PtrInt(Text)+L)^,BufferLen);
end;

function AppendRawUTF8ToBuffer(Buffer: PUTF8Char; const Text: RawUTF8): PUTF8Char;
var L: PtrInt;
begin
  L := PtrInt(Text);
  if L<>0 then begin
    L := pInteger(L-4)^; // L := length(Text)
    Move(Pointer(Text)^,Buffer^,L);
    inc(Buffer,L);
  end;
  result := Buffer;
end;

function QuotedStr(const S: RawUTF8; Quote: AnsiChar): RawUTF8;
begin
  result := QuotedStr(Pointer(S),Quote);
end;

function QuotedStr(Text: PUTF8Char; Quote: AnsiChar): RawUTF8; overload;
var n, L: integer;
    P: PUTF8Char;
begin
  n := 0;
  L := 0;
  if Text<>nil then begin
    P := Text;
    while P^<>#0 do begin
      if P^=Quote then
        inc(n);
      inc(P);
      inc(L);
    end;
  end;
  SetString(Result,nil,L+n+2);
  P := pointer(Result);
  P^ := Quote;
  inc(P);
  if n=0 then begin
    Move(Text^,P^,L);
    inc(P,L);
  end else
  repeat
    if Text^=Quote then begin
      PWord(P)^ := ord(Quote)+ord(Quote) shl 8;
      inc(P,2);
    end else begin
      P^ := Text^;
      inc(P);
    end;
    inc(Text);
  until Text^=#0;
  P^ := Quote;
  //Assert(P-pointer(Result)+1=length(result));
end;

function GotoEndOfQuotedString(P: PUTF8Char): PUTF8Char;
var quote: AnsiChar;
begin
  quote := P^;
  inc(P);
  repeat
    if P^=#0 then
      break;
    if P^<>quote then
      inc(P) else
      if P[1]=quote then // allow double quotes inside string
        inc(P,2) else begin
        inc(P);
        break; // end quote
      end;
  until false;
  result := P;
end;

function GotoNextNotSpace(P: PUTF8Char): PUTF8Char;
begin
  if P^ in [#1..' '] then
    repeat
      inc(P)
    until not(P^ in [#1..' ']);
  result := P;
end;

function UnQuoteSQLString(P: PUTF8Char; out Value: RawUTF8): PUTF8Char;
var quote: AnsiChar;
    PBeg, PS: PUTF8Char;
    n: PtrInt;
begin
  if P=nil then begin
    result := nil;
    exit;
  end;
  quote := P^;
  inc(P);
  // compute unquoted string length
  PBeg := P;
  n := 0;
  repeat
    if P^=#0 then
      break;
    if P^<>quote then
      inc(P) else
      if P[1]=quote then begin
        inc(P,2); // allow double quotes inside string
        inc(n);
      end else
        break; // end quote
  until false;
  if P^=#0 then begin
    result := nil; // end of string before end quote -> incorrect
    exit;
  end;
  // create unquoted string
  if n=0 then
    // no quote within
    SetString(Value,PAnsiChar(PBeg),P-PBeg) else begin
    // unescape internal quotes
    SetLength(Value,P-PBeg-n);
    P := PBeg;
    PS := Pointer(Value);
    repeat
      if P^=quote then
        if P[1]=quote then
          inc(P) else // allow double quotes inside string
          break; // end quote
      PS^ := P^;
      inc(PS);
      inc(P);
    until false;
  end;
  result := P+1;
end;

function isSelect(P: PUTF8Char): boolean;
begin
  if P<>nil then begin
    P := SQLBegin(P);
    result :=
      (IdemPChar(P,'SELECT') or IdemPChar(P,'VACUUM') or IdemPChar(P,'PRAGMA')) and
      (P[6] in [#0..' ',';']); // avoid VACUUMStoredProcUnsecure security issue
  end else
    result := true; // assume '' statement is SELECT command
end;

function SQLBegin(P: PUTF8Char): PUTF8Char;
begin
  if P<>nil then
  repeat
    if P^<=' ' then // ignore blanks
      repeat
        if P^=#0 then
          break else
          inc(P)
      until P^>' ';
    if PWord(P)^=ord('-')+ord('-')shl 8 then // SQL comments
      repeat
        inc(P)
      until P^ in [#0,#10]
    else
    if PWord(P)^=ord('/')+ord('*')shl 8 then begin // C comments
      inc(P);
      repeat
        inc(P);
        if PWord(P)^=ord('*')+ord('/')shl 8 then begin
          inc(P,2);
          break;
        end;
      until P^=#0;
    end
    else break;
 until false;
 result := P;
end;


{$ifndef OWNI2S}

function Int32ToUTF8(Value : integer): RawUTF8; // faster than SysUtils.IntToStr
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt32(@tmp[15],Value);
  SetString(result,P,@tmp[15]-P);
end;

function Int64ToUtf8(Value: Int64): RawUTF8; // faster than SysUtils.IntToStr
var tmp: array[0..23] of AnsiChar;
    P: PAnsiChar;
begin
{$ifdef CPU64}
  P := StrInt32(@tmp[23],Value); // StrInt32 use PtrInt, i.e. Int64
{$else}
  P := StrInt64(@tmp[23],Value);
{$endif}
  SetString(result,P,@tmp[23]-P);
end;

{$endif}

function UInt32ToUTF8(Value : Cardinal): RawUTF8; // faster than SysUtils.IntToStr
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrUInt32(@tmp[15],Value);
  SetString(result,P,@tmp[15]-P);
end;

{.$define EXTENDEDTOSTRING_USESTR}
// see http://synopse.info/fossil/tktview?name=6593f0fbd1

{$ifndef WITHUXTHEME}
  {$define EXTENDEDTOSTRING_USESTR} // no TFormatSettings before Delphi 6
{$endif}

{$ifdef LVCL}
  {$define EXTENDEDTOSTRING_USESTR} // no FloatToText implemented in LVCL
{$endif}

{$ifdef FPC}
  {$define EXTENDEDTOSTRING_USESTR} // FloatToText alls str() in FPC
{$endif}

{$ifndef EXTENDEDTOSTRING_USESTR}
var
  // standard FormatSettings (US)
  V_Format_Settings: TFormatSettings;
{$endif}

function ExtendedToString(var S: ShortString; Value: Extended; Precision: integer): integer;
{$ifdef EXTENDEDTOSTRING_USESTR}
var i,prec: integer;
{$else}
{$ifdef UNICODE}
var i: integer;
{$endif}
{$endif}
begin
{$ifdef EXTENDEDTOSTRING_USESTR}
  str(Value:0:Precision,S); // not str(Value:0,S) -> '  0.0E+0000'
  // using str() here avoid FloatToStrF() usage -> LVCL is enough
  result := length(S);
  prec := result; // if no decimal
  if S[1]='-' then
    dec(prec);
  for i := 2 to result do // test if scientific format -> return as this
    case S[i] of
    'E': exit;  // pos('E',S)>0; which Delphi 2009+ don't like
    '.': dec(prec);
    end;
  if (prec>Precision) and (prec<>result) then begin
    dec(result,prec-Precision);
    if S[result+1]>'5' then begin // manual rounding
      prec := result;
      repeat
        case S[prec] of
        '.':  dec(prec);
        '0'..'8': begin
          inc(S[prec]);
          break;
        end;
        '9': S[prec] := '0';
        else break;
        end;
        dec(prec);
      until prec=0;
    end; // note: this fixes http://stackoverflow.com/questions/2335162
  end;
  while S[result]='0' do begin
    dec(result); // trunc any trimming 0
    if S[result]='.' then begin
      dec(result);
      if (result=2) and (S[1]='-') and (S[2]='0') then begin
        result := 1;
        S[1] := '0'; // '-0.000' -> '0'
      end;
      break; // decimal were all '0' -> return only integer part
    end;
  end;
{$else}
  // use ffGeneral: see http://synopse.info/forum/viewtopic.php?pid=442#p442
  result := FloatToText(PChar(@S[1]), Value, fvExtended, ffGeneral,
    Precision, 0, V_Format_Settings);
  {$ifdef UNICODE} // FloatToText(PWideChar) is faster than FloatToText(PAnsiChar)
  for i := 1 to result do
    PByteArray(@S)[i] := PWordArray(PtrInt(@S)-1)[i];
  {$endif}
{$endif EXTENDEDTOSTRING_USESTR}
end;

function ExtendedToStr(Value: Extended; Precision: integer): RawUTF8;
var tmp: ShortString;
begin
  SetString(result,PAnsiChar(@tmp[1]),ExtendedToString(tmp,Value,Precision));
end;

function DoubleToStr(Value: Double): RawUTF8;
var tmp: ShortString;
begin
  if Value=0 then
    result := '0' else
    SetString(result,PAnsiChar(@tmp[1]),ExtendedToString(tmp,Value,DOUBLE_PRECISION));
end;

procedure VarRecToUTF8(const V: TVarRec; var result: RawUTF8);
begin 
  with V do
  case V.VType of
    vtString:     result := VString^;
    vtAnsiString: result := RawUTF8(VAnsiString); // expect UTF-8 content
{$ifdef UNICODE}
    vtUnicodeString: result := UnicodeStringToUtf8(string(VUnicodeString));
{$endif}
    vtWideString: result := RawUnicodeToUtf8(VWideString,length(WideString(VWideString)));
    vtPChar:      result := RawUTF8(VPChar);
    vtChar:       result := RawUTF8(VChar);
    vtPWideChar:  result := RawUnicodeToUtf8(VPWideChar,StrLenW(VPWideChar));
    vtWideChar:   result := RawUnicodeToUtf8(@VWideChar,1);
    vtBoolean:    if VBoolean then result := '1' else result := '0';
    vtInteger:    result :=
      {$ifndef ENHANCEDRTL}Int32ToUtf8{$else}IntToStr{$endif}(VInteger);
    vtInt64:      result :=
      {$ifndef ENHANCEDRTL}Int64ToUtf8{$else}IntToStr{$endif}(VInt64^);
    vtCurrency:   result := Curr64ToStr(VInt64^);
    vtExtended:   result := ExtendedToStr(VExtended^,DOUBLE_PRECISION);
    vtPointer:    result := PointerToHex(VPointer);
    vtClass:      result := PShortString(PPointer(PtrInt(VClass)+vmtClassName)^)^;
    else result := '';
  end;
end;

function FormatUTF8(Format: PUTF8Char; const Args: array of const): RawUTF8;
// only supported token is %, with any const arguments
var i, tmpN, L, A: PtrInt;
    tmp: array[0..24] of RawUTF8; // enough for 12 Args + inbetween text
    PDeb: PUTF8Char;
label Txt, Add;
begin
  if (Format='') or (high(Args)<0) then begin
    result := Format;
    exit;
  end;
  result := '';
  tmpN := 0;
  L := 0;
  A := 0;
  while Format^<>#0 do begin
    if Format^<>'%' then begin
      PDeb := Format;
      while (Format^<>'%') and (Format^<>#0) do inc(Format);
Txt:  inc(L,Format-PDeb);
      SetString(tmp[tmpN],PDeb,Format-PDeb); // add inbetween text
      inc(tmpN);
      if tmpN=length(tmp) then
        break;
    end;
    if Format^=#0 then break;
    inc(Format); // jump '%'
    if A<=high(Args) then begin
      VarRecToUTF8(Args[A],tmp[tmpN]);
      inc(L,length(tmp[tmpN]));
      inc(A);
      inc(tmpN);
      if tmpN=length(tmp) then
        goto Add;
    end else
Add:if Format^<>#0 then begin // no more available Args -> add all remaining text
      PDeb := Format;
      repeat inc(Format) until (Format^=#0);
      goto Txt;
    end;
  end;
  if L=0 then
    exit;
  SetLength(result,L);
  Format := pointer(result);
  for i := 0 to tmpN-1 do
  if tmp[i]<>'' then begin
    L := PInteger(PtrInt(tmp[i])-4)^;
    move(pointer(tmp[i])^,Format^,L);
    inc(Format,L);
  end;  
end;

function RawByteStringArrayConcat(const Values: array of RawByteString): RawByteString;
var i, L: integer;
    P: PAnsiChar;
begin
  L := 0;
  for i := 0 to high(Values) do
    inc(L,length(Values[i]));
  SetString(Result,nil,L);
  P := pointer(Result);
  for i := 0 to high(Values) do begin
    L := length(Values[i]);
    move(pointer(Values[i])^,P^,L);
    inc(P,L);
  end;
end;

function FormatUTF8(Format: PUTF8Char; const Args, Params: array of const): RawUTF8; overload;
// support both % and ? tokens
var i, tmpN, L, A, P: PtrInt;
    isParam: AnsiChar;
    tmp: array[0..24] of RawUTF8; // enough for 12 Args + inbetween text
    inlin: set of 0..24; // high(tmp) does not work with XE2 !
    PDeb: PUTF8Char;
label Txt, Inl, Add;
begin
  if (Format='') or ((high(Args)<0)and(high(Params)<0)) then begin
    result := Format; // no formatting to handle
    exit;
  end;
  result := '';
  tmpN := 0;
  integer(inlin) := 0;
  L := 0;
  A := 0;
  P := 0;
  while Format^<>#0 do begin
    if Format^<>'%' then begin
      PDeb := Format;
      while not (Format^ in [#0,'%','?']) do inc(Format);
Txt:  inc(L,Format-PDeb);
      SetString(tmp[tmpN],PDeb,Format-PDeb); // add inbetween text
      inc(tmpN);
      if tmpN=length(tmp) then
        break;
    end;
    if Format^=#0 then
      break;
    isParam := Format^;
    inc(Format); // jump '%' or '?'
    if (isParam='%') and (A<=high(Args)) then begin
      VarRecToUTF8(Args[A],tmp[tmpN]);
      inc(A);
Inl:  inc(L,length(tmp[tmpN]));
      inc(tmpN);
      if tmpN=length(tmp) then
        goto Add;
    end else
    if (isParam='?') and (P<=high(Params)) then begin // add inlined :(...):
      VarRecToUTF8(Params[P],tmp[tmpN]);
      if not (Params[P].VType in [vtBoolean,vtInteger,vtInt64,vtCurrency,vtExtended]) then
        tmp[tmpN] := QuotedStr(pointer(tmp[tmpN]),'''');
      inc(L,4); // space for :():
      include(inlin,tmpN);
      inc(P);
      goto Inl;
    end else
Add:if Format^<>#0 then begin // no more available Args -> add all remaining text
      PDeb := Format;
      repeat inc(Format) until (Format^=#0);
      goto Txt;
    end;
  end;
  if L=0 then
    exit;
  SetLength(result,L);
  Format := pointer(result);
  for i := 0 to tmpN-1 do
  if tmp[i]<>'' then
    if i in inlin then begin
      PWord(Format)^ := ord(':')+ord('(')shl 8;
      inc(Format,2);
      L := PInteger(PtrInt(tmp[i])-4)^;
      move(pointer(tmp[i])^,Format^,L);
      inc(Format,L);
      PWord(Format)^ := ord(')')+ord(':')shl 8;
      inc(Format,2);
    end else begin
      L := PInteger(PtrInt(tmp[i])-4)^;
      move(pointer(tmp[i])^,Format^,L);
      inc(Format,L);
    end;
end;

function StrIComp(Str1, Str2: PUTF8Char): PtrInt;
{$ifdef PUREPASCAL}
var C1, C2: AnsiChar;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    repeat
      C1 := Str1^;
      C2 := Str2^;
      if C1 in ['a'..'z'] then dec(C1,32);
      if C2 in ['a'..'z'] then dec(C2,32);
      if (C1<>C2) or (C1=#0) then
        break;
      Inc(Str1);
      Inc(Str2);
    until false;
    Result := Ord(C1) - Ord(C2);
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
asm // faster version by AB
        mov     ecx,eax
        xor     eax,eax
        cmp     ecx,edx
        je      @exit2  // same string or both nil
        or      ecx,ecx
        mov     al,1
        jz      @exit2  // str1=''
        or      edx,edx
        je      @min
@1:     mov     al,[ecx]
        inc     ecx
        test    al,al
        mov     ah,[edx]
        lea     edx,[edx+1]
        je      @exit
        cmp     al,ah
        je      @1
        sub     al,'a'
        sub     ah,'a'
        cmp     al,'z'-'a'
        ja      @@2
        sub     al,20h
@@2:    cmp     ah,'z'-'a'
        ja      @@3
        sub     ah,20h
@@3:    cmp     al,ah
        je      @1
@exit:  xor     edx,edx
        xchg    ah,dl
        sub     eax,edx
@exit2: ret
@min:   or      eax,-1
end;
{$endif}

function StrLenW(S: PWideChar): PtrInt;
begin
  result := 0;
  if S<>nil then
  while true do
    if S[0]<>#0 then
    if S[1]<>#0 then
    if S[2]<>#0 then
    if S[3]<>#0 then begin
      inc(S,4);
      inc(result,4);
    end else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;

function StrLen(S: PUTF8Char): PtrInt;
{$ifdef PUREPASCAL}
begin
  result := 0;
  if S<>nil then
  while true do
    if S[0]<>#0 then
    if S[1]<>#0 then
    if S[2]<>#0 then
    if S[3]<>#0 then begin
      inc(S,4);
      inc(result,4);
    end else begin
      inc(result,3);
      exit;
    end else begin
      inc(result,2);
      exit;
    end else begin
      inc(result);
      exit;
    end else
      exit;
end;
{$else}
// faster than default SysUtils version
asm
     test eax,eax
     jz @@z
     cmp   byte ptr [eax+0],0; je @@0
     cmp   byte ptr [eax+1],0; je @@1
     cmp   byte ptr [eax+2],0; je @@2
     cmp   byte ptr [eax+3],0; je @@3
     push  eax
     and   eax,-4              { DWORD Align Reads }
@@Loop:
     add   eax,4
     mov   edx,[eax]           { 4 Chars per Loop }
     lea   ecx,[edx-$01010101]
     not   edx
     and   edx,ecx
     and   edx,$80808080       { Set Byte to $80 at each #0 Position }
     jz    @@Loop              { Loop until any #0 Found }
@@SetResult:
     pop   ecx
     bsf   edx,edx             { Find First #0 Position }
     shr   edx,3               { Byte Offset of First #0 }
     add   eax,edx             { Address of First #0 }
     sub   eax,ecx             { Returns Length }
@@z: ret
@@0: xor eax,eax; ret
@@1: mov eax,1;   ret
@@2: mov eax,2;   ret
@@3: mov eax,3
end;
{$endif}

function StrCompW(Str1, Str2: PWideChar): PtrInt;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if Str1^=Str2^ then
    repeat
      if (Str1^=#0) or (Str2^=#0) then break;
      inc(Str1);
      inc(Str2);
    until Str1^<>Str2^;
    result := pWord(Str1)^-pWord(Str2)^;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;

function StrComp(Str1, Str2: PUTF8Char): PtrInt;
{$ifdef PUREPASCAL}
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then begin
    if Str1^=Str2^ then
    repeat
      if (Str1^=#0) or (Str2^=#0) then break;
      inc(Str1);
      inc(Str2);
    until Str1^<>Str2^;
    result := pByte(Str1)^-pByte(Str2)^;
  end else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
asm // faster version by AB
        mov     ecx,eax
        xor     eax,eax
        cmp     ecx,edx
        je      @exit2  // same string or both nil
        or      ecx,ecx
        mov     al,1
        jz      @exit2  // Str1=''
        or      edx,edx
        je      @min
@1:     mov     al,[ecx]
        inc     ecx
        mov     ah,[edx]
        inc     edx
        test    al,al
        je      @exit
        cmp     al,ah
        je      @1
@exit:  xor     edx,edx
        xchg    ah,dl
        sub     eax,edx
@exit2: ret
@min:   or      eax,-1
end;
{$endif}


function IdemPropNameU(const P1,P2: RawUTF8): boolean;
{$ifdef PUREPASCAL}
var i,L: integer;
begin
  result := false;
  L := length(P1);
  if L<>length(P2) then
    exit;
  for i := 0 to L-1 do
    if (ord(P1[i+1]) xor ord(P2[i+1])) and $DF<>0 then
      exit;
  result := true;
end;
{$else}
asm
    cmp eax,edx
    jz @ok // same string
    or eax,eax
    jz @z  // P1=nil -> false
    or edx,edx
    jz @z  // P2=nil -> false
    mov ecx,[eax-4]
    cmp ecx,[edx-4]
    jne @z      // length differs
@1: mov ch,[eax]
    xor ch,[edx]
    and ch,$DF   // case insensitive compare
    jne @z
    dec cl
    lea eax,[eax+1]
    lea edx,[edx+1]
    jnz @1
@ok:mov al,1
    ret
@z: xor eax,eax
end;
{$endif}

function IdemPropName(const P1,P2: shortstring): boolean; overload;
var i,j: integer;
begin
  result := false;
  if P1[0]<>P2[0] then
    exit;
  j := 1;
  for i := 1 to ord(P1[0]) shr 2 do
    if (PCardinal(@P1[j])^ xor PCardinal(@P2[j])^) and $dfdfdfdf<>0 then
      exit else
      inc(j,4);
  for i := j to ord(P1[0]) do
    if (ord(P1[i]) xor ord(P2[i])) and $df<>0 then
      exit;
  result := true;
end;

function IdemPropName(const P1: shortstring; P2: PUTF8Char; P2Len: integer): boolean; overload;
var i,j: integer;
begin
  result := false;
  if ord(P1[0])<>P2Len then
    exit;
  dec(P2);
  j := 1;
  for i := 1 to P2Len shr 2 do
    if (PCardinal(@P1[j])^ xor PCardinal(@P2[j])^) and $dfdfdfdf<>0 then
      exit else
      inc(j,4);
  for i := j to P2Len do
    if (ord(P1[i]) xor ord(P2[i])) and $df<>0 then
      exit;
  result := true;
end;

procedure InitSynCommonsConversionTables;
var i: integer;
    v: byte;
{$ifdef OWNNORMTOUPPER}
    d: integer;
const n2u: array[138..255] of byte =
  (83,139,140,141,90,143,144,145,146,147,148,149,150,151,152,153,83,155,140,
   157,90,89,160,161,162,163,164,165,166,167,168,169,170,171,172,173,174,175,
   176,177,178,179,180,181,182,183,184,185,186,187,188,189,190,191,65,65,65,
   65,65,65,198,67,69,69,69,69,73,73,73,73,68,78,79,79,79,79,79,215,79,85,85,
   85,85,89,222,223,65,65,65,65,65,65,198,67,69,69,69,69,73,73,73,73,68,78,79,
   79,79,79,79,247,79,85,85,85,85,89,222,89);
{$endif OWNNORMTOUPPER}
begin
{$ifndef EXTENDEDTOSTRING_USESTR}
  {$ifdef ISDELPHIXE}
  //SettingsUS := TFormatSettings.Create($0409);
 // SettingsUS := TFormatSettings.Create(LOCALE_SYSTEM_DEFAULT);
  {$else}
  //GetLocaleFormatSettings($0409,SettingsUS);
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT,V_Format_Settings);
  {$endif}
{$endif}
  for i := 0 to 255 do
  begin
    if i in [ord('a')..ord('z'), ord('�')..ord('�')] then
       NormToUpperAnsi7Byte[i] := i-32
    else   
      NormToUpperAnsi7Byte[i] := i;
  end;
  {for i := ord('a') to ord('z') do
    dec(NormToUpperAnsi7Byte[i],32);
  
  for i := ord('�') to ord('�') do
    dec(NormToUpperAnsi7Byte[i],32); }
{$ifdef OWNNORMTOUPPER}
  // initialize custom NormToUpper[] and NormToLower[] arrays
  move(NormToUpperAnsi7,NormToUpper,138);
  move(n2u,NormToUpperByte[138],sizeof(n2u));
  for i := 0 to 255 do begin
    d := NormToUpperByte[i];
    if d in [ord('A')..ord('Z'), ord('�')..ord('�')] then
      inc(d,32);
    NormToLowerByte[i] := d;
  end;
{$endif OWNNORMTOUPPER}
  // code below is 55 bytes long, therefore shorter than a const array
  fillchar(ConvertHexToBin[0],sizeof(ConvertHexToBin),255); // all to 255
  v := 0;
  for i := ord('0') to ord('9') do begin
    ConvertHexToBin[i] := v;
    inc(v);
  end;
  for i := ord('A') to ord('F') do begin
    ConvertHexToBin[i] := v;
    ConvertHexToBin[i+(ord('a')-ord('A'))] := v;
    inc(v);
  end;
  // initialize our internaly used TSynAnsiConvert engines
  WinAnsiConvert := TSynAnsiConvert.Engine(CODEPAGE_RU) as TSynAnsiFixedWidth;
  CurrentAnsiConvert := TSynAnsiConvert.Engine(GetACP);
end;

{$ifdef MSWINDOWS}
const
  // lpMinimumApplicationAddress retrieved from Windows is very low $10000
  // - i.e. maximum number of ID per table would be 65536 in TSQLRecord.GetID
  // - so we'll force an higher and almost "safe" value as 1,048,576
  // (real value from runnning Windows is greater than $400000)
  MIN_PTR_VALUE = $100000;

  // see http://msdn.microsoft.com/en-us/library/ms724833(v=vs.85).aspx
  VER_NT_WORKSTATION = 1;
  VER_NT_DOMAIN_CONTROLLER = 2;
  VER_NT_SERVER = 3;
  SM_SERVERR2 = 89;
  PROCESSOR_ARCHITECTURE_AMD64 = 9;

{$ifndef UNICODE}
function GetVersionEx(var lpVersionInformation: TOSVersionInfoEx): BOOL; stdcall;
  external kernel32 name 'GetVersionExA';
{$endif}

procedure RetrieveSystemInfo;
var
  IsWow64Process: function(Handle: THandle; var Res: BOOL): BOOL; stdcall;
  GetNativeSystemInfo: procedure(var SystemInfo: TSystemInfo); stdcall;
  Res: BOOL;
  Kernel: THandle;
  P: pointer;
  Vers: TWindowsVersion;
begin
  Kernel := GetModuleHandle(kernel32);
  IsWow64Process := GetProcAddress(Kernel,'IsWow64Process');
  Res := false;
  IsWow64 := Assigned(IsWow64Process) and
    IsWow64Process(GetCurrentProcess,Res) and Res;
  fillchar(SystemInfo,sizeof(SystemInfo),0);
  if IsWow64 then // see http://msdn.microsoft.com/en-us/library/ms724381(v=VS.85).aspx
    GetNativeSystemInfo := GetProcAddress(Kernel,'GetNativeSystemInfo') else
    @GetNativeSystemInfo := nil;
  if Assigned(GetNativeSystemInfo) then
    GetNativeSystemInfo(SystemInfo) else
    WinApi.Windows.GetSystemInfo(SystemInfo);
  GetMem(P,10); // ensure that using MIN_PTR_VALUE won't break anything
  if (PtrUInt(P)>MIN_PTR_VALUE) and
     (PtrUInt(SystemInfo.lpMinimumApplicationAddress)<=MIN_PTR_VALUE) then
     PtrUInt(SystemInfo.lpMinimumApplicationAddress) := MIN_PTR_VALUE;
  Freemem(P);
  OSVersionInfo.dwOSVersionInfoSize := sizeof(OSVersionInfo);
  GetVersionEx(OSVersionInfo);
  Vers := wUnknown;
  with OSVersionInfo do
  case dwMajorVersion of
    5: case dwMinorVersion of
     0: Vers := w2000;
     1: Vers := wXP;
     2: if (wProductType=VER_NT_WORKSTATION) and
           (SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64) then
          Vers := wXP_64 else
        if GetSystemMetrics(SM_SERVERR2)=0 then
          Vers := wServer2003 else
          Vers := wServer2003_R2;
    end;
    6: case dwMinorVersion of
     0: if wProductType=VER_NT_WORKSTATION then
          Vers := wVista else
          Vers := wServer2008;
     1: if wProductType=VER_NT_WORKSTATION then
          Vers := wSeven else
          Vers := wServer2008_R2;
     2: if wProductType=VER_NT_WORKSTATION then
          Vers := wEight else
          Vers := wEightServer;
    end;
  end;
  if SystemInfo.wProcessorArchitecture=PROCESSOR_ARCHITECTURE_AMD64 then
    inc(Vers);
  OSVersion := Vers;
end;
{$endif MSWINDOWS}


procedure SoundExComputeAnsi(var p: PAnsiChar; var result: cardinal; Values: PSoundExValues);
var n,v,old: cardinal;
begin
  n := 0;
  old := 0;
  if Values<>nil then
  repeat
{$ifdef USENORMTOUPPER}
    v := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (1252 accents)
{$else}
    v := NormToUpperAnsi7Byte[ord(p^)]; // 7 bit char uppercase
{$endif}
    if not (v in IsWord) then break;
    inc(p);
    dec(v,ord('B'));
    if v>high(TSoundExValues) then continue;
    v := Values[v]; // get soundex value
    if (v=0) or (v=old) then continue; // invalid or dopple value
    old := v;
    result := result shl SOUNDEX_BITS;
    inc(result,v);
    inc(n);
    if n=((32-8)div SOUNDEX_BITS) then // first char use up to 8 bits
      break; // result up to a cardinal size
  until false;
end;

function SoundExComputeFirstCharAnsi(var p: PAnsiChar): cardinal;
label Err;
begin
  if p=nil then begin
Err:result := 0;
    exit;
  end;
  repeat
{$ifdef USENORMTOUPPER}
    result := NormToUpperByte[ord(p^)]; // also handle 8 bit WinAnsi (CP 1252)
{$else}
    result := NormToUpperAnsi7Byte[ord(p^)]; // 7 bit char uppercase
{$endif}
    if result=0 then
      goto Err; // end of input text, without a word
    inc(p);
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G','I'..'Z'];
end;

function GetNextUTF8Upper(var U: PUTF8Char): cardinal;
begin
  result := ord(U^);
  if result<>0 then
  if result and $80=0 then begin
    inc(U);
{$ifdef USENORMTOUPPER}
      result := NormToUpperByte[result];  {$else}
      result := NormToUpperAnsi7Byte[result]); {$endif}
    exit;
  end else
  if result and $20=0 then begin
    result := result shl 6+byte(U[1])-$00003080;
    inc(U,2);
    if (result<=255) and (WinAnsiConvert.AnsiToWide[result]<=255) then
  {$ifdef USENORMTOUPPER}
        result := NormToUpperByte[result]  {$else}
        result := NormToUpperAnsi7Byte[result]) {$endif} else
      result := ord('?'); // char ignored for soundex
    exit;
  end else begin
    inc(U,3);
    result := ord('?');   // char ignored for soundex
    exit;
  end else
    exit;
end;

procedure SoundExComputeUTF8(var U: PUTF8Char; var result: cardinal; Values: PSoundExValues);
var n,v,old: cardinal;
begin
  n := 0;
  old := 0;
  if Values<>nil then
  repeat
    v := GetNextUTF8Upper(U);
    if not (v in IsWord) then break;
    dec(v,ord('B'));
    if v>high(TSoundExValues) then continue;
    v := Values[v]; // get soundex value
    if (v=0) or (v=old) then continue; // invalid or dopple value
    old := v;
    result := result shl SOUNDEX_BITS;
    inc(result,v);
    inc(n);
    if n=((32-8)div SOUNDEX_BITS) then // first char use up to 8 bits
      break; // result up to a cardinal size
  until false;
end;

function SoundExComputeFirstCharUTF8(var U: PUTF8Char): cardinal;
label Err;
begin
  if U=nil then begin
Err:result := 0;
    exit;
  end;
  repeat
    result := GetNextUTF8Upper(U);
    if result=0 then
      goto Err; // end of input text, without a word
    // trim initial spaces or 'H'
  until AnsiChar(result) in ['A'..'G','I'..'Z'];
end;

function FindNextUTF8WordBegin(U: PUTF8Char): PUTF8Char;
var c: cardinal;
    V: PUTF8Char;
begin
  result := nil;
  repeat
    c := GetNextUTF8Upper(U);
    if c=0 then
      exit;
  until not(c in IsWord);
  repeat
    V := U;
    c := GetNextUTF8Upper(U);
    if c=0 then
      exit;
  until c in IsWord;
  result := V;
end;


{ TSynSoundEx }

const
  /// english Soundex pronunciation scores
  // - defines the default values used for the SoundEx() function below
  // (used if Values parameter is nil)
  ValueEnglish: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,1,2,0,0,2,2,4,5,5,0,1,2,6,2,3,0,1,0,2,0,2);

  /// french Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueFrench: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,9,7,0,0,7,2,4,5,5,0,1,2,6,8,3,0,9,0,8,0,8);

  /// spanish Soundex pronunciation scores
  // - can be used to override default values used for the SoundEx()
  // function below
  ValueSpanish: TSoundExValues =
  // B C D E F G H I J K L M N O P Q R S T U V W X Y Z
    (1,2,3,0,1,2,0,0,0,2,0,5,5,0,1,2,6,2,3,0,1,0,2,0,2);

  SOUNDEXVALUES: array[TSynSoundExPronunciation] of PSoundExValues =
    (@ValueEnglish,@ValueFrench,@ValueSpanish,@ValueEnglish);

function TSynSoundEx.Ansi(A: PAnsiChar): boolean;
var Value, c: cardinal;
begin
  result := false;
  if A=nil then exit;
  repeat
    // test beginning of word
    c := SoundExComputeFirstCharAnsi(A);
    if c=0 then exit else
    if c=FirstChar then begin
      // here we had the first char match -> check if word match UpperValue
      Value := c-(ord('A')-1);
      SoundExComputeAnsi(A,Value,fValues);
      if Value=search then begin
        result := true; // UpperValue found!
        exit;
      end;
    end else
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
        if not(NormToUpperByte[ord(A^)] in IsWord) then break else inc(A);
{$else} if not(ord(A^) in IsWord) then break else inc(A); {$endif}
    until false;
    // find beginning of next word
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
        if NormToUpperByte[ord(A^)] in IsWord then break else inc(A);
{$else} if ord(A^) in IsWord then break else inc(A); {$endif}
    until false;
  until false;
end;

function TSynSoundEx.UTF8(U: PUTF8Char): boolean;
var Value, c: cardinal;
    V: PUTF8Char;
begin
  result := false;
  if U=nil then exit;
  repeat
    // find beginning of word
    c := SoundExComputeFirstCharUTF8(U);
    if c=0 then exit else
    if c=FirstChar then begin
      // here we had the first char match -> check if word match UpperValue
      Value := c-(ord('A')-1);
      SoundExComputeUTF8(U,Value,fValues);
      if Value=search then begin
        result := true; // UpperValue found!
        exit;
      end;
    end else
    repeat
      c := GetNextUTF8Upper(U);
      if c=0 then
        exit;
    until not(c in IsWord);
    // find beginning of next word
    repeat
      if U=nil then exit;
      V := U;
      c := GetNextUTF8Upper(U);
      if c=0 then
        exit;
    until c in IsWord;
    U := V;
  until U=nil;
end;

function TSynSoundEx.Prepare(UpperValue: PAnsiChar; Lang: TSynSoundExPronunciation): boolean;
begin
  fValues := SOUNDEXVALUES[Lang];
  Search := SoundExAnsi(UpperValue,nil,Lang);
  if Search=0 then
    result := false else begin
    FirstChar := SoundExComputeFirstCharAnsi(UpperValue);
    result := true;
  end;
end;

function SoundExAnsi(A: PAnsiChar; next: PPAnsiChar;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExComputeFirstCharAnsi(A);
  if result<>0 then begin
    dec(result,ord('A')-1);   // first Soundex char is first char
    SoundExComputeAnsi(A,result,SOUNDEXVALUES[Lang]);
  end;
  if next<>nil then begin
{$ifdef USENORMTOUPPER}
    while NormToUpperByte[ord(A^)] in IsWord do inc(A); // go to end of word
{$else}
    while ord(A^) in IsWord do inc(A); // go to end of word
{$endif}
    next^ := A;
  end;
end;

function SoundExUTF8(U: PUTF8Char; next: PPUTF8Char;
  Lang: TSynSoundExPronunciation): cardinal;
begin
  result := SoundExComputeFirstCharUTF8(U);
  if result<>0 then begin
    dec(result,ord('A')-1);   // first Soundex char is first char
    SoundExComputeUTF8(U,result,SOUNDEXVALUES[Lang]);
  end;
  if next<>nil then
    next^ := FindNextUTF8WordBegin(U);
end;

{$ifdef USENORMTOUPPER}

function AnsiICompW(u1, u2: PWideChar): PtrInt;
begin
  if u1<>u2 then
    if u1<>nil then
      if u2<>nil then
        repeat
          result := PtrInt(u1^)-PtrInt(u2^);
          if result<>0 then begin
            if (PtrInt(u1^)>255) or (PtrInt(u2^)>255) then exit;
            result := NormToUpperAnsi7Byte[PtrInt(u1^)]-NormToUpperAnsi7Byte[PtrInt(u2^)];
            if result<>0 then exit;
          end;
          if (u1^=#0) or (u2^=#0) then break;
          inc(u1);
          inc(u2);
        until false else
        result := 1 else  // u2=''
      result := -1 else // u1=''
    result := 0;      // u1=u2
end;


{$ifdef PUREPASCAL}
function AnsiIComp(Str1, Str2: PWinAnsiChar): PtrInt;
begin
  if Str1<>Str2 then
  if Str1<>nil then
  if Str2<>nil then
  repeat
    result := NormToUpperByte[ord(Str1^)]-NormToUpperByte[pByte(Str2)^];
    if result<>0 then exit;
    if (Str1^=#0) or (Str2^=#0) then break;
    inc(Str1);
    inc(Str2);
  until false else
  result := 1 else  // Str2=''
  result := -1 else // Str1=''
  result := 0;      // Str1=Str2
end;
{$else}
function AnsiIComp(Str1, Str2: PWinAnsiChar): PtrInt;
asm // fast 8 bits WinAnsi comparaison using the NormToUpper[] array
    cmp eax,edx
    je @2
    test eax,edx // is either of the strings perhaps nil?
    jz @3
@0: push ebx // compare the first character (faster quicksort)
    movzx ebx,byte ptr [eax] // ebx=S1[1]
    movzx ecx,byte ptr [edx] // ecx=S2[1]
    or ebx,ebx
    jz @z
    cmp ebx,ecx
    je @s
    mov bl,byte ptr [NormToUpper+ebx]
    mov cl,byte ptr [NormToUpper+ecx]
    cmp ebx,ecx
    je @s
    mov eax,ebx
    pop ebx
    sub eax,ecx // return S1[1]-S2[1]
    ret
@2: xor eax, eax
    ret
@3: test eax,eax // S1=''
    jz @4
    test edx,edx // S2='' ?
    jnz @0
    mov eax,1 // return 1 (S1>S2)
    ret
@s: inc eax
    inc edx
    mov bl,[eax] // ebx=S1[i]
    mov cl,[edx] // ecx=S2[i]
    or ebx,ebx
    je @z        // end of S1
    cmp ebx,ecx
    je @s
    mov bl,byte ptr [NormToUpper+ebx]
    mov cl,byte ptr [NormToUpper+ecx]
    cmp ebx,ecx
    je @s
    mov eax,ebx
    pop ebx
    sub eax,ecx // return S1[i]-S2[i]
    ret
@z: cmp ebx,ecx // S1=S2?
    pop ebx
    jz @2
@4: or eax,-1 // return -1 (S1<S2)
end;
{$endif}

function ConvertCaseUTF8(P: PUTF8Char; const Table: TNormTableByte): PtrInt;
var D: PUTF8Char;
    c: PtrUInt;
label nxt;
begin // code below is very well optimized
  result := 0;
  if P=nil then
    exit;
  D := P;
  repeat
    c := byte(P[0]);
    if c=0 then
      break;
    inc(P);
    if c and $80=0 then begin
      D[result] := AnsiChar(Table[c]);
      inc(result);
    end else
    if c and $20=0  then begin // UTF-8 decode
      c := c shl 6+byte(P[0])-$00003080;
      inc(P);
      if (c<=255) and (WinAnsiConvert.AnsiToWide[c]<=255) then begin
        c := WinAnsiConvert.AnsiToWide[Table[c]]; // convert to Unicode
        if c<=127 then begin
          D[result] := AnsiChar(Table[c]);
          inc(result);
        end else
          goto nxt; // leave UTF-8 encoding untouched
        continue;
      end;
nxt:  PWord(D+result)^ := PWord(P-2)^; // leave UTF-8 encoding untouched
      inc(result,2);
    end else begin
      PInteger(D+result)^ := PInteger(P-1)^; // leave UTF-8 encoding untouched
      inc(P,2);
      inc(result,3);
    end;
  until false;
end;

function UpperCaseU(const S: RawUTF8): RawUTF8;
var LS,LD: integer;
begin
  LS := length(S);
  SetString(result,PAnsiChar(pointer(S)),LS);
  LD := ConvertCaseUTF8(pointer(result),NormToUpperByte);
  if LS<>LD then
    SetLength(result,LD);
end;

function LowerCaseU(const S: RawUTF8): RawUTF8;
var LS,LD: integer;
begin
  LS := length(S);
  SetString(result,PAnsiChar(pointer(S)),LS);
  LD := ConvertCaseUTF8(pointer(result),NormToLowerByte);
  if LS<>LD then
    SetLength(result,LD);
end;

function UTF8IComp(u1, u2: PUTF8Char): PtrInt;
var c2: PtrInt;
    b: byte;
begin // fast UTF-8 comparaison using the NormToUpper[] array for all 8 bits values
  if u1<>u2 then
  if u1<>nil then
  if u2<>nil then
  repeat
    result := pbyte(u1)^;
    inc(u1);
    if result and $80=0 then
      if result<>0 then begin
        result := NormToUpperByte[result];
        b := pByte(u2)^;
        if b and $80=0 then begin
          if b=0 then exit; // u1>u2 -> return u1^
          dec(result,NormToUpperByte[b]);
          inc(u2);
          if result<>0 then exit;
          continue;
        end;
      end else begin // end of u1 reached
        if u2^<>#0 then    // end of u2 reached -> u1=u2 -> return 0
          result := -1;    // u1<u2
        exit;
      end else begin
        if result and $20=0 then begin
          result := result shl 6+pbyte(u1)^-$00003080;
          inc(u1);
        end else begin
          result := result shl 6+pbyte(u1)^;
          inc(u1);
          result := result shl 6+pbyte(u1)^-$000E2080;
          inc(u1);
        end;
        if result and $ffffff00=0 then
          result := NormToUpperByte[result]; // 8 bits to upper, 32 bits as is
      end;
    c2 := pbyte(u2)^; inc(u2);
    if c2 and $80=0 then begin
      if c2=0 then exit; // u1>u2 -> return u1^
      dec(result,NormToUpperByte[c2]);
      if result<>0 then exit;
    end else begin
      if c2 and $20=0 then begin
        c2 := c2 shl 6+pbyte(u2)^-$00003080;
        inc(u2);
      end else begin
        c2 := c2 shl 6+pbyte(u2)^;
        inc(u2);
        c2 := c2 shl 6+pbyte(u2)^-$000E2080;
        inc(u2);
      end;
      if c2 and $ffffff00=0 then
        dec(result,NormToUpperByte[c2]) else // 8 bits to upper
        dec(result,c2); // 32 bits widechar returns diff
      if result<>0 then exit;
    end;
  until false else
  result := 1 else  // u2=''
  result := -1 else // u1=''
  result := 0;      // u1=u2
end;

function UTF8ILComp(u1, u2: PUTF8Char; L1,L2: cardinal): PtrInt;
var c2: PtrInt;
label neg,pos,eq;
begin // fast UTF-8 comparaison using the NormToUpper[] array for all 8 bits values
  if u1<>u2 then
  if (u1<>nil) and (L1<>0) then
  if (u2<>nil) and (L2<>0) then
  repeat
    result := pByte(u1)^;
    inc(u1);
    dec(L1);
    if result and $80=0 then begin
      result := NormToUpperByte[result];
      if pByte(u2)^ and $80=0 then begin
        dec(result,NormToUpperByte[pByte(u2)^]);
        dec(L2);
        inc(u2);
        if result<>0 then
           exit else
        if L1<>0 then
          if L2<>0 then
            continue else // L1>0 and L2>0 -> next char
            goto pos else // L1>0 and L2=0 -> u1>u2
          if L2<>0 then
            goto neg else // L1=0 and L2>0 -> u1<u2
            exit;         // L1=0 and L2=0 -> u1=u2
      end;
    end else begin
      if L1=0 then goto neg else dec(L1);
      if result and $20=0 then begin
        result := result shl 6+pByte(u1)^-$00003080;
        inc(u1);
      end else begin
        if L1=0 then goto neg else dec(L1);
        result := result shl 6+pByte(u1)^;
        inc(u1);
        result := result shl 6+pByte(u1)^-$000E2080;
        inc(u1);
      end;
      if result and $ffffff00=0 then
        result := NormToUpperByte[result]; // 8 bits to upper, 32 bits as is
    end;
    // here result=NormToUpper[u1^]
    c2 := pByte(u2)^;
    inc(u2);
    dec(L2);
    if c2 and $80=0 then begin
      dec(result,NormToUpperByte[c2]);
      if result<>0 then exit;
    end else begin
      if L2=0 then goto pos else dec(L2);
      if c2 and $20=0 then begin
        c2 := c2 shl 6+pByte(u2)^-$00003080;
        inc(u2);
      end else begin
        if L2=0 then goto pos else dec(L2);
        c2 := c2 shl 6+pByte(u2)^;
        inc(u2);
        c2 := c2 shl 6+pByte(u2)^-$000E2080;
        inc(u2);
      end;
      if c2 and $ffffff00=0 then
        dec(result,NormToUpperByte[c2]) else // 8 bits to upper
        dec(result,c2); // returns 32 bits diff
      if result<>0 then exit;
    end;
    // here we have result=NormToUpper[u2^]-NormToUpper[u1^]=0
    if L1=0 then // test if we reached end of u1 or end of u2
      if L2=0 then exit     // u1=u2
         else goto neg else // u1<u2
    if L2=0 then goto pos;  // u1>u2
  until false else
pos: result := 1 else  // u2='' or u1>u2
neg: result := -1 else // u1='' or u1<u2
     result := 0;      // u1=u2
end;

function SameTextU(const S1, S2: RawUTF8): Boolean;
// this code calls UTF8IComp, which uses NormToUpper[] after UTF-8 decoding
// checking lengths is not to be performed in case of UTF-8
begin
  result := UTF8IComp(pointer(S1),pointer(S2))=0;
end;

{$else} // no NormToUpper[]

function AnsiIComp(Str1, Str2: PWinAnsiChar): integer;
{$ifdef PUREPASCAL}
begin
  result := StrIComp(Str1,Str2); // fast enough
end;
{$else}
asm
  jmp StrIComp // LVCL without NormToUpper[]: use default SysUtils implementation
end;
{$endif}
{$endif}

function FindAnsi(A, UpperValue: PAnsiChar): boolean;
var ValueStart: PAnsiChar;
{$ifndef USENORMTOUPPER}
    ch: AnsiChar;
{$endif}
begin
  result := false;
  if (A=nil) or (UpperValue=nil) then exit;
  ValueStart := UpperValue;
  repeat
    // test beginning of word
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
      if byte(NormToUpper[A^]) in IsWord then break else inc(A);  {$else}
      if byte(NormToUpperAnsi7[A^]) in IsWord then break else inc(A);
{$endif}
    until false;
    // check if this word is the UpperValue
    UpperValue := ValueStart;
    repeat
{$ifdef USENORMTOUPPER}
      if NormToUpper[A^]<>UpperValue^ then break;  {$else}
      if NormToUpperAnsi7[A^]<>UpperValue^ then break;
{$endif}
      inc(UpperValue);
      if UpperValue^=#0 then begin
        result := true; // UpperValue found!
        exit;
      end;
      inc(A);
      if A^=#0 then exit;
    until false;
    // find beginning of next word
    repeat
      if A^=#0 then exit else
{$ifdef USENORMTOUPPER}
        if not (NormToUpperByte[ord(A^)] in IsWord) then break else inc(A);
{$else} if not (ord(A^) in IsWord) then break else inc(A); {$endif}
    until false;
  until false;
end;

function FindUnicode(PW, Upper: PWideChar; UpperLen: integer): boolean;
var Start: PWideChar;
begin
  result := false;
  if (PW=nil) or (Upper=nil) then exit;
  repeat
    // go to beginning of next word
    repeat
      if ord(PW^)=0 then exit else
      if (ord(PW^)>126) or (ord(PW^) in IsWord) then
        Break;
      inc(PW);
    until false;
    Start := PW;
    // search end of word matching UpperLen characters
    repeat
      inc(PW);
    until (PW-Start>=UpperLen) or
      (ord(PW^)=0) or ((ord(PW^)<126) and (not(ord(PW^) in IsWord)));
    if PW-Start>=UpperLen then
      if CompareStringW(LOCALE_USER_DEFAULT,NORM_IGNORECASE,Start,UpperLen,Upper,UpperLen)=2 then begin
        result := true; // match found
        exit;
      end;
    // not found: go to end of current word
    repeat
      if PW^=#0 then exit else
      if ((ord(PW^)<126) and (not(ord(PW^) in IsWord))) then Break;
      inc(PW);
    until false;
  until false;
end;

function FindUTF8(U: PUTF8Char; UpperValue: PAnsiChar): boolean;
var ValueStart: PAnsiChar;
{$ifdef USENORMTOUPPER}
    c: cardinal;
    FirstChar: AnsiChar;
label Next;
{$else}
    ch: AnsiChar;
{$endif}
begin
  result := false;
  if (U=nil) or (UpperValue=nil) then exit;
{$ifdef USENORMTOUPPER}
  // handles 8-bits WinAnsi chars inside UTF-8 encoded data
  FirstChar := UpperValue^;
  ValueStart := UpperValue+1;
  repeat
    // test beginning of word
    repeat
      c := byte(U^); inc(U);
      if c=0 then exit else
      if c and $80=0 then begin
        if c in IsWord then
          if PAnsiChar(@NormToUpper)[c]<>FirstChar then
            goto Next else
            break;
      end else
      if c and $20=0 then begin
        c := c shl 6+byte(U^)-$00003080;
        inc(U);
        if c<=255 then begin
          c := NormToUpperByte[c];
          if c in IsWord then
            if AnsiChar(c)<>FirstChar then
              goto Next else
              break;
        end;
      end else
        inc(U,2);
    until false;
    // here we had the first char match -> check if this word match UpperValue
    UpperValue := ValueStart;
    repeat
      if UpperValue^=#0 then begin
        result := true; // UpperValue found!
        exit;
      end;
      c := byte(U^); inc(U); // next chars
      if c=0 then exit else
      if c and $80=0 then begin
        if PAnsiChar(@NormToUpper)[c]<>UpperValue^ then break;
      end else
      if c and $20=0 then begin
        c := c shl 6+byte(U^)-$00003080;
        inc(U);
        if (c>255) or (PAnsiChar(@NormToUpper)[c]<>UpperValue^) then break;
      end else begin
        inc(U,2);
        break;
      end;
      inc(UpperValue);
    until false;
    // find beginning of next word
Next:
{$else}
  // this tiny version only handles 7-bits ansi chars and ignore all UTF-8 chars
  ValueStart := UpperValue;
  repeat
    // find beginning of word
    repeat
      if byte(U^)=0 then exit else
      if byte(U^) and $80=0 then
        if byte(U^) in IsWord then
          break else
          inc(U) else
      if byte(U^) and $20=0 then
        inc(U,2) else
        inc(U,3);
    until false;
    // check if this word is the UpperValue
    UpperValue := ValueStart;
    repeat
      ch := NormToUpperAnsi7[U^];
      if ch<>UpperValue^ then break;
      inc(UpperValue);
      if UpperValue^=#0 then begin
        result := true; // UpperValue found!
        exit;
      end;
      inc(U);
      if byte(U^)=0 then exit else
      if byte(U^) and $80<>0 then break; // 7 bits char check only
    until false;
{$endif}
    // find beginning of next word
    U := FindNextUTF8WordBegin(U);
  until U=nil;
end;

function HexDisplayToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: integer): boolean;
var B,C: byte;
    i: integer;
begin
  result := false; // return false if any invalid char
  inc(Bin,BinBytes-1);
  for i := 1 to BinBytes do begin
    B := ConvertHexToBin[Ord(Hex^)];
    inc(Hex);
    if B>15 then exit;
    C := ConvertHexToBin[Ord(Hex^)];
    Inc(Hex);
    if C>15 then exit;
    Bin^ := B shl 4+C;
    Dec(Bin);
  end;
  result := true; // correct content in Hex
end;

function HexDisplayToCardinal(Hex: PAnsiChar; out aValue: cardinal): boolean;
begin
  result := HexDisplayToBin(Hex,@aValue,sizeof(aValue));
end;

function HexToBin(Hex: PAnsiChar; Bin: PByte; BinBytes: Integer): boolean;
var I: Integer;
    B,C: byte;
begin
  result := false; // return false if any invalid char
  if Bin<>nil then
  for I := 1 to BinBytes do begin
    B := ConvertHexToBin[Ord(Hex^)];
    inc(Hex);
    if B>15 then exit;
    C := ConvertHexToBin[Ord(Hex^)];
    Inc(Hex);
    if C>15 then exit;
    Bin^ := B shl 4+C;
    Inc(Bin);
  end else
  for I := 1 to BinBytes do begin // no Bin^ -> just validate Hex^ Stream format
    B := ConvertHexToBin[Ord(Hex^)];
    inc(Hex);
    if B>15 then exit;
    C := ConvertHexToBin[Ord(Hex^)];
    Inc(Hex);
    if C>15 then exit;
  end;
  result := true; // conversion OK
end;

const
  b64: array[0..63] of AnsiChar =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

function Base64EncodeMain(rp, sp: PAnsiChar; len: cardinal): integer;
{$ifdef PUREPASCAL}
  {$ifdef HASINLINE}inline;{$endif}
var i: integer;
    c: cardinal;
begin
  result := len div 3;
  for i := 1 to result do begin
    c := ord(sp[0]) shl 16 + ord(sp[1]) shl 8 + ord(sp[2]);
    rp[0] := b64[(c shr 18) and $3f];
    rp[1] := b64[(c shr 12) and $3f];
    rp[2] := b64[(c shr 6) and $3f];
    rp[3] := b64[c and $3f];
    inc(rp,4);
    inc(sp,3);
  end;
end;
{$else}
asm // eax=rp edx=sp ecx=len - pipeline optimized version by AB 
    push ebx
    push esi
    push edi
    push ebp
    mov ebx,edx
    xor edx,edx
    mov esi,eax
    mov eax,ecx
    lea ecx,[edx+3]
    mov edi,offset b64
    div ecx
    or eax,eax
    mov ebp,eax
    push eax
    jz @z
    // edi=b64 ebx=sp esi=rp ebp=len div 3
    xor eax,eax
    nop
@1: // read 3 bytes from sp
    movzx edx,byte ptr [ebx]
    shl edx,16
    mov al,[ebx+2]
    mov ah,[ebx+1]
    lea ebx,[ebx+3]
    or eax,edx
    // encode as Base64
    mov ecx,eax
    mov edx,eax
    shr ecx,6
    and edx,$3F
    and ecx,$3F
    mov dh,[edi+edx]
    mov dl,[edi+ecx]
    mov ecx,eax
    shr eax,12
    shr ecx,18
    shl edx,16
    and ecx,$3F
    and eax,$3F
    mov cl,[edi+ecx]
    mov ch,[edi+eax]
    or ecx,edx
    // write the 4 encoded bytes into rp
    dec ebp
    mov [esi],ecx
    lea esi,[esi+4]
    jnz @1
@z: pop eax // result := len div 3
    pop ebp
    pop edi
    pop esi
    pop ebx
end;

{$endif}

procedure Base64EncodeTrailing(rp, sp: PAnsiChar; len: cardinal);
  {$ifdef HASINLINE}inline;{$endif}
var c: cardinal;
begin
  case len of
    1: begin
      c := ord(sp[0]) shl 4;
      rp[0] := b64[(c shr 6) and $3f];
      rp[1] := b64[c and $3f];
      rp[2] := '=';
      rp[3] := '=';
    end;
    2: begin
      c := ord(sp[0]) shl 10 + ord(sp[1]) shl 2;
      rp[0] := b64[(c shr 12) and $3f];
      rp[1] := b64[(c shr 6) and $3f];
      rp[2] := b64[c and $3f];
      rp[3] := '=';
    end;
  end;
end;

procedure Base64Encode(rp, sp: PAnsiChar; len: cardinal);
var main: cardinal;
begin
  main := Base64EncodeMain(rp,sp,len);
  Base64EncodeTrailing(rp+main*4,sp+main*3,len-main*3);
end;

function BinToBase64(const s: RawByteString): RawByteString;
var len: integer;
begin
  result:='';
  len := length(s);
  if len = 0 then exit;
  SetLength(result,((len + 2) div 3) * 4);
  Base64Encode(pointer(result),pointer(s),len);
end;

function BinToBase64(Bin: PAnsiChar; BinBytes: integer): RawByteString; 
begin
  result:='';
  if BinBytes = 0 then exit;
  SetLength(result,((BinBytes + 2) div 3) * 4);
  Base64Encode(pointer(result),Bin,BinBytes);
end;

function BinToBase64URI(Bin: PAnsiChar; BinBytes: integer): RawByteString;
var P: PUTF8Char;
begin
  result := BinToBase64(Bin,BinBytes);
  P := pointer(result);
  if P<>nil then
    repeat
      case P^ of
      #0: break;
      '+': P^ := '-';
      '/': P^ := '_';
      '=': begin // trim unsignificant trailing '=' characters
        SetLength(result,P-pointer(result));
        break;
      end;
      end;
      inc(P);
    until false;
end;

function BinToBase64WithMagic(const s: RawByteString): RawByteString;
var len: integer;
begin
  result:='';
  len := length(s);
  if len=0 then
    exit;
  SetLength(result,((len+2) div 3)*4+3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result))+3,pointer(s),len);
end;

function BinToBase64WithMagic(Data: pointer; DataLen: integer): RawByteString; overload;
begin
  result:='';
  if DataLen<=0 then
    exit;
  SetLength(result,((DataLen+2) div 3)*4+3);
  PInteger(pointer(result))^ := JSON_BASE64_MAGIC;
  Base64Encode(PAnsiChar(pointer(result))+3,Data,DataLen);
end;

var
  /// a conversion table from Base64 text into binary data
  // - used by Base64ToBin function
  ConvertBase64ToBin: array of shortint;

procedure InitConvertBase64ToBin;
var i: integer;
begin
  SetLength(ConvertBase64ToBin,256);
  fillchar(ConvertBase64ToBin[0],256,255); // invalid value set to -1
  for i := 0 to high(b64) do
    ConvertBase64ToBin[ord(b64[i])] := i;
  ConvertBase64ToBin[ord('=')] := -2; // special value for '='
end;

type
  TConvertBase64ToBinTable = array[AnsiChar] of shortint;

function IsBase64(sp: PAnsiChar; len: PtrInt): boolean;
var i: PtrInt;
    Table: ^TConvertBase64ToBinTable;
begin
  result := false;
  if ConvertBase64ToBin=nil then
    InitConvertBase64ToBin;
  if (len=0) or (len and 3<>0) then
    exit;
  Table := pointer(ConvertBase64ToBin);
  for i := 0 to len-5 do
    if Table[sp[i]]<0 then
      exit;
  inc(sp,len-4);
  if (Table[sp[0]]=-1) or // -2 = '=' is allowed here
     (Table[sp[1]]=-1) or (Table[sp[2]]=-1) or (Table[sp[3]]=-1) then
      exit;
  result := true; // layout seems correct
end;

function IsBase64(const s: RawByteString): boolean;
begin
  result := IsBase64(pointer(s),length(s));
end;

function Base64ToBinLength(sp: PAnsiChar; len: PtrInt): PtrInt;
var Table: ^TConvertBase64ToBinTable absolute ConvertBase64ToBin;
begin
  if (len=0) or (len and 3<>0) then begin
    result := 0;
    exit;
  end;
  if ConvertBase64ToBin=nil then
    InitConvertBase64ToBin;
  if Table[sp[len-2]]>=0 then
    if Table[sp[len-1]]>=0 then
      result := 0 else
      result := 1 else
      result := 2;
  result := (len shr 2)*3-result;
end;

procedure Base64Decode(sp,rp: PAnsiChar; len: PtrInt);
{$ifdef PUREPASCAL}
var i: PtrInt;
    c, ch: PtrInt;
    Table: ^TConvertBase64ToBinTable;
begin
  Table := pointer(ConvertBase64ToBin);
  for i := 1 to len do begin
    c := Table[sp[0]];
    if c>=0 then begin
      c := c shl 6;
      ch := Table[sp[1]];
      if ch>=0 then begin
        c := (c or ch) shl 6;
        ch := Table[sp[2]];
        if ch>=0 then begin
          c := (c or ch) shl 6;
          ch := Table[sp[3]];
          if ch>=0 then begin
            c := c or ch;
            rp[2] := AnsiChar(c);
            c := c shr 8;
            rp[1] := AnsiChar(c);
            c := c shr 8;
            rp[0] := AnsiChar(c);
            inc(rp,3);
            inc(sp,4);
            continue;
          end else begin
            c := c shr 8;
            rp[1] := AnsiChar(c);
            rp[0] := AnsiChar(c shr 8);
            //assert(resultlen=len*3-1);
            exit;
          end;
        end;
      end;
    end;
    rp[0] := AnsiChar(c shr 10);
    //assert(resultlen=len*3-2);
    exit;
  end;
end;
{$else}
asm // eax=sp edx=rp ecx=len - pipeline optimized version by AB
     push ebx
     push esi
     push edi
     push ebp
     push eax
     or ecx,ecx
     mov ebp,edx
     mov edi,dword ptr [ConvertBase64ToBin]
     mov [esp],ecx
     jz @4
     xor edx,edx
     xor ebx,ebx
@0:  mov dl,[eax]
     mov bl,[eax+$01]
     movsx ecx,byte ptr [edi+edx]
     movsx esi,byte ptr [edi+ebx]
     test ecx,ecx
     jl @1
     shl ecx,$06
     test esi,esi
     jl @1
     or ecx,esi
     mov dl,[eax+$02]
     mov bl,[eax+$03]
     shl ecx,$06
     movsx esi,byte ptr [edi+edx]
     movsx edx,byte ptr [edi+ebx]
     test esi,esi
     jl @1
     or ecx,esi
     shl ecx,$06
     test edx,edx
     jl @2
     or ecx,edx
     lea eax,[eax+4]
     mov [ebp+2],cl
     mov [ebp+1],ch
     shr ecx,16
     dec dword ptr [esp]
     mov [ebp],cl
     lea ebp,[ebp+3]
     jnz @0
@4:  pop eax
     pop ebp
     pop edi
     pop esi
     pop ebx
     ret
@2:  shr ecx,$08
     mov [ebp+$01],cl
     mov [ebp],ch
     jmp @4
@1:  shr ecx,$0a
     mov [ebp],cl
     jmp @4
end;
{$endif}

function Base64ToBin(const s: RawByteString): RawByteString;
var len, resultLen: PtrInt;
begin
  len := length(s);
  resultLen := Base64ToBinLength(pointer(s),len);
  if resultLen=0 then
    result := '' else begin
    SetString(result,nil,resultLen);
    Base64Decode(pointer(s),pointer(result),len shr 2);
  end;
end;

function Base64ToBin(sp: PAnsiChar; len: PtrInt): RawByteString;
var resultLen: PtrInt;
begin
  resultLen := Base64ToBinLength(sp,len);
  if resultLen=0 then
    result := '' else begin
    SetString(result,nil,resultLen);
    Base64Decode(sp,pointer(result),len shr 2);
  end;
end;

function UpperCaseUnicode(const S: RawUTF8): RawUTF8;
{$ifdef MSWINDOWS}
var tmp: RawUnicode;
    TmpLen: integer;
{$endif}
begin
{$ifdef MSWINDOWS}
  tmp := Utf8DecodeToRawUnicodeUI(S,@TmpLen);
  TmpLen := TmpLen shr 1;
  CharUpperBuffW(pointer(tmp),TmpLen);
  Result := RawUnicodeToUtf8(pointer(tmp),TmpLen);
{$endif}
{$ifdef POSIX}
  result := WideStringToUTF8(WideUpperCase(UTF8ToWideString(S)));
{$endif}
end;

function LowerCaseUnicode(const S: RawUTF8): RawUTF8;
{$ifdef MSWINDOWS}
var tmp: RawUnicode;
    TmpLen: integer;
{$endif}
begin
{$ifdef MSWINDOWS}
  tmp := Utf8DecodeToRawUnicodeUI(S,@TmpLen);
  TmpLen := TmpLen shr 1;
  CharLowerBuffW(pointer(tmp),TmpLen);
  Result := RawUnicodeToUtf8(pointer(tmp),TmpLen);
{$endif}
{$ifdef POSIX}
  result := WideStringToUTF8(WideLowerCase(UTF8ToWideString(S)));
{$endif}
end;

function UpperCase(const S: RawUTF8): RawUTF8;
var L, i: PtrInt;
    P: PUTF8Char;
begin
  L := length(S);
  SetString(Result,PAnsiChar(pointer(S)),L);
  P := pointer(Result);
  for i := 0 to L-1 do
    if P[i] in ['a'..'z', '�'..'�'] then
      dec(P[i],32);
end;

function LowerCase(const S: RawUTF8): RawUTF8;
var L, i: PtrInt;
    P: PUTF8Char;
begin
  L := length(S);
  SetString(Result,PAnsiChar(pointer(S)),L);
  P := pointer(Result);
  for i := 0 to L-1 do
    if P[i] in ['A'..'Z','�'..'�'] then
      inc(P[i],32);
end;

function TrimLeft(const S: RawUTF8): RawUTF8;
var i, l: Integer;
begin
  l := Length(S);
  i := 1;
  while (i <= l) and (S[i] <= ' ') do
    Inc(i);
  Result := Copy(S, i, Maxint);
end;

function TrimRight(const S: RawUTF8): RawUTF8;
var i: Integer;
begin
  i := Length(S);
  while (i > 0) and (S[i] <= ' ') do
    Dec(i);
  SetString(result,PAnsiChar(pointer(S)),i);
end;

const
  /// fast lookup table for converting hexadecimal numbers from 0 to 15
  // into their ASCII equivalence
  // - our enhanced SysUtils.pas (normal and LVCL) contains the same array
  // - should be local for better code generation
  HexChars: array[0..15] of AnsiChar = '0123456789ABCDEF';

procedure BinToHex(Bin, Hex: PAnsiChar; BinBytes: integer);
var j, v: cardinal;
begin
  for j := 1 to BinBytes do begin
    v := byte(Bin^);
    Hex[0] := HexChars[v shr 4];
    Hex[1] := HexChars[v and $F];
    inc(Hex,2);
    inc(Bin);
  end;
end;

function BinToHex(const Bin: RawByteString): RawUTF8; overload;
var L: integer;
begin
  L := length(Bin);
  SetString(result,nil,L*2);
  SynCommons2.BinToHex(pointer(Bin),pointer(Result),L);
end;

procedure BinToHexDisplay(Bin, Hex: PAnsiChar; BinBytes: integer);
var j, v: cardinal;
begin
  for j := BinBytes-1 downto 0 do begin
    v := byte(Bin^);
    Hex[j*2] := HexChars[v shr 4];
    Hex[j*2+1] := HexChars[v and $F];
    inc(Bin);
  end;
end;

function PointerToHex(aPointer: Pointer): RawUTF8;
begin
  SetString(result,nil,sizeof(Pointer)*2);
  BinToHexDisplay(aPointer,pointer(Result),sizeof(Pointer));
end;

function CardinalToHex(aCardinal: Cardinal): RawUTF8;
begin
  SetString(result,nil,sizeof(Cardinal)*2);
  BinToHexDisplay(@aCardinal,pointer(Result),sizeof(Cardinal));
end;

procedure YearToPChar(Y: Word; P: PUTF8Char);
{$ifdef PUREPASCAL}
begin
  PWord(P  )^ := TwoDigitLookupW[Y div 100];
  PWord(P+2)^ := TwoDigitLookupW[Y mod 100];
end;
{$else}
asm
  mov cl,100
  div cl // ah=remainder=Y mod 100, al=quotient=Year div 100
  movzx ecx,al // al=quotient=Y div 100
  mov cx,word ptr [TwoDigitLookup+ecx*2]
  mov [edx],cx
  movzx ecx,ah // ah=remainder=Y mod 100
  mov cx,word ptr [TwoDigitLookup+ecx*2]
  mov [edx+2],cx
end;
{$endif}

function SameValue(const A, B: Double; DoublePrec: double = 1E-12): Boolean;
var AbsA,AbsB: double;
begin // faster than the Math unit version
  AbsA := Abs(A);
  AbsB := Abs(B);
  if AbsA<AbsB then
    AbsA := AbsA*DoublePrec else
    AbsA := AbsB*DoublePrec; // AbsA := Min(Abs(A),Abs(B))*DoublePrec
  // AbsA is the allowed Epsilon value
  if AbsA<DoublePrec then
    Result := Abs(A-B)<=DoublePrec else
    Result := Abs(A-B)<=AbsA;
end;

/// return the index of Value in Values[], -1 if not found
function FindRawUTF8(const Values: TRawUTF8DynArray; const Value: RawUTF8;
  CaseSensitive: boolean=true): integer;
begin
  if CaseSensitive then begin
    for result := 0 to high(Values) do
      if Values[result]=Value then
        exit;
  end else
    for result := 0 to high(Values) do
      if SameTextU(Values[result],Value) then
        exit;
  result := -1;
end;

/// true if Value was added successfully in Values[]
function AddRawUTF8(var Values: TRawUTF8DynArray; const Value: RawUTF8;
  NoDuplicates: boolean=false; CaseSensitive: boolean=true): boolean;
var i: integer;
begin
  if NoDuplicates then begin
    i := FindRawUTF8(Values,Value,CaseSensitive);
    if i>=0 then begin
      result := false;
      exit;
    end;
  end;
  i := length(Values);
  SetLength(Values,i+1);
  Values[i] := Value;
  result := true;
end;

function RawUTF8DynArrayEquals(const A,B: TRawUTF8DynArray): boolean;
var i: integer;
begin
  result := false;
  if length(A)<>length(B) then
    exit;
  for i := 0 to high(A) do
    if A[i]<>B[i] then
      exit;
  result := true;
end;

const
  ANSICHARNOT01310: set of AnsiChar = [#1..#9,#11,#12,#14..#255];

/// find the position of the SEARCH] section in source
// - return true if SEARCH] was found, and store line after it in source
function FindSectionFirstLine(var source: PUTF8Char; search: PUTF8Char): boolean;
{$ifdef PUREPASCAL}
begin
  result := false;
  if source=nil then
    exit;
  repeat
    if source^='[' then begin
      inc(source);
      result := IdemPChar(source,search);
    end;
    while source^ in ANSICHARNOT01310 do inc(source);
    while source^ in [#10,#13] do inc(source);
    if result then
      exit; // found
  until source^=#0;
  source := nil;
end;
{$else}
asm // eax=source edx=search
    push eax       // save source var
    mov eax,[eax]  // eax=source
    or eax,eax
    jz @z
    push ebx
    mov ebx,edx    // save search
    cmp byte ptr [eax],'['
    lea eax,[eax+1]
    jne @s
@i: push eax
    mov edx,ebx   // edx=search
    call IdemPChar
    pop ecx       // ecx=source
    jmp @1
@s: mov ecx,eax
    xor eax,eax   // result := false
@1: mov dl,[ecx]  // while not (source^ in [#0,#10,#13]) do inc(source);
    inc ecx
    cmp dl,13
    ja @1
    je @e
    or dl,dl
    jz @0
    cmp dl,10
    jne @1
    jmp @4
@e: cmp byte ptr [ecx],10 // jump #13#10
    jne @4
    inc ecx
@4: test al,al
    jnz @x         // exit if IdemPChar returned true
    cmp byte ptr [ecx],'['
    lea ecx,[ecx+1]
    jne @1
    mov eax,ecx
    jmp @i
@0: xor ecx,ecx    // set source=nil
@x: pop ebx
    pop edx        // restore source var
    mov [edx],ecx  // update source var
    ret
@z: pop edx       // ignore source var, result := false
end;
{$endif}

{$ifdef USENORMTOUPPER}
{$ifdef PUREPASCAL}
function IdemPCharW(p: pWideChar; up: PUTF8Char): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    if (p^>#255) or (up^<>AnsiChar(NormToUpperByte[ord(p^)])) then
      exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;
{$else}
function IdemPCharW(p: pWideChar; up: PUTF8Char): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
// eax=p edx=up
asm
  or eax,eax
  jz @e // P=nil -> false
  or edx,edx
  push ebx
  push esi
  jz @z // up=nil -> true
  mov esi,offset NormToUpper
  xor ebx,ebx
  xor ecx,ecx
@1:
  mov bx,[eax] // bl=p^
  mov cl,[edx] // cl=up^
  or bh,bh     // p^ > #255 -> FALSE
  jnz @n
  test cl,cl
  mov bl,[ebx+esi] // bl=NormToUpper[p^]
  jz @z // up^=#0 -> OK
  lea edx,[edx+1] // = inc edx without changing flags
  cmp bl,cl
  lea eax,[eax+2]
  je @1
@n:
  pop esi
  pop ebx
  xor eax,eax
@e:
  ret
@z:
  mov al,1 // up^=#0 -> OK
  pop esi
  pop ebx
end;
{$endif}
{$else}
function IdemPCharW(p: pWideChar; up: PUTF8Char): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    if (p^>#255) or (up^<>AnsiChar(NormToUpperByteAnsi7[ord(p^)])) then
      exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;
{$endif}

function FindSectionFirstLineW(var source: PWideChar; search: PUTF8Char): boolean;
{$ifdef PUREPASCAL}
begin
  result := false;
  if source=nil then
    exit;
  repeat
    if source^='[' then begin
      inc(source);
      result := IdemPCharW(source,search);
    end;
    while not (cardinal(source^) in [0,10,13]) do inc(source);
    while cardinal(source^) in [10,13] do inc(source);
    if result then
      exit; // found
  until source^=#0;
  source := nil;
end;
{$else}
asm // eax=source edx=search
    push eax       // save source var
    mov eax,[eax]  // eax=source
    or eax,eax
    jz @z
    push ebx
    mov ebx,edx    // save search
    cmp word ptr [eax],'['
    lea eax,[eax+2]
    jne @s
@i: push eax
    mov edx,ebx   // edx=search
    call IdemPCharW
    pop ecx       // ecx=source
    jmp @1
@s: mov ecx,eax
    xor eax,eax   // result := false
@1: mov dx,[ecx]  // while not (source^ in [#0,#10,#13]) do inc(source);
    lea ecx,[ecx+2]
    cmp dx,13
    ja @1
    je @e
    or dx,dx
    jz @0
    cmp dx,10
    jne @1
    jmp @4
@e: cmp word ptr [ecx],10 // jump #13#10
    jne @4
    lea ecx,[ecx+2]
@4: test al,al
    jnz @x         // exit if IdemPChar returned true
    cmp word ptr [ecx],'['
    lea ecx,[ecx+2]
    jne @1
    mov eax,ecx
    jmp @i
@0: xor ecx,ecx    // set source=nil
@x: pop ebx
    pop edx        // restore source var
    mov [edx],ecx  // update source var
    ret
@z: pop edx       // ignore source var, result := false
end;
{$endif}

/// find the Value of UpperName in P, till end of current section
// - expect UpperName as 'NAME='
function FindIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): RawUTF8;
var PDeb: PUTF8Char;
    L: integer;
begin
  while (P<>nil) and (P^<>'[') do begin
    PDeb := GetNextLineBegin(P,P); // since PDeb=P, we have PDeb<>nil
    if PDeb^=' ' then repeat inc(PDeb) until PDeb^<>' ';   // trim left ' '
    if IdemPChar(PDeb,UpperName) then begin
      inc(PDeb,StrLen(UpperName));
      L := 0; while PDeb[L]>=' ' do inc(L); // get line length
      SetString(result,PDeb,L);
      exit;
    end;
  end;
  result := '';
end;

function ExistsIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): boolean;
var PDeb: PUTF8Char;
begin
  result := true;
  while (P<>nil) and (P^<>'[') do begin
    PDeb := GetNextLineBegin(P,P); // since PDeb=P, we have PDeb<>nil
    if PDeb^=' ' then repeat inc(PDeb) until PDeb^<>' ';   // trim left ' '
    if IdemPChar(PDeb,UpperName) then
      exit;
  end;
  result := false;
end;

function FindWinAnsiIniNameValue(P: PUTF8Char; UpperName: PUTF8Char): RawUTF8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniNameValue(P,UpperName)));
end;

function ExistsIniName(P: PUTF8Char; UpperName: PUTF8Char): boolean;
var PDeb: PUTF8Char;
begin
  result := true;
  while (P<>nil) and (P^<>'[') do begin
    PDeb := GetNextLineBegin(P,P); // since PDeb=P, we have PDeb<>nil
    if PDeb^=' ' then repeat inc(PDeb) until PDeb^<>' ';   // trim left ' '
    if IdemPChar(PDeb,UpperName) then
      exit;
  end;
  result := false;
end;

function GetSectionContent(SectionFirstLine: PUTF8Char): RawUTF8;
var PDeb: PUTF8Char;
begin
  PDeb := SectionFirstLine;
  while (SectionFirstLine<>nil) and (SectionFirstLine^<>'[') do
    GetNextLineBegin(SectionFirstLine,SectionFirstLine);
  if SectionFirstLine=nil then
    result := PDeb else
    SetString(result,PDeb,SectionFirstLine-PDeb);
end;

function GetSectionContent(const Content, SectionName: RawUTF8): RawUTF8; overload;
var P: PUTF8Char;
    UpperSection: array[byte] of AnsiChar;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection,SectionName))^ := ord(']');
  if FindSectionFirstLine(P,UpperSection) then
    result := GetSectionContent(P) else
    result := '';
end;

function DeleteSection(var Content: RawUTF8; const SectionName: RawUTF8;
  EraseSectionHeader: boolean=true): boolean;
var P: PUTF8Char;
    UpperSection: array[byte] of AnsiChar;
begin
  result := false; // no modification
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection,SectionName))^ := ord(']');
  if FindSectionFirstLine(P,UpperSection) then
    result := DeleteSection(P,Content,EraseSectionHeader);
end;

function DeleteSection(SectionFirstLine: PUTF8Char; var Content: RawUTF8;
  EraseSectionHeader: boolean=true): boolean;
var PEnd: PUTF8Char;
    IndexBegin: PtrInt;
begin
  result := false;
  PEnd := SectionFirstLine;
  if EraseSectionHeader then // erase [Section] header line
    while (PtrUInt(SectionFirstLine)>PtrUInt(Content)) and (SectionFirstLine^<>'[') do dec(SectionFirstLine);
  while (PEnd<>nil) and (PEnd^<>'[') do
    GetNextLineBegin(PEnd,PEnd);
  IndexBegin := SectionFirstLine-pointer(Content);
  if IndexBegin=0 then
    exit; // no modification
  if PEnd=nil then
    SetLength(Content,IndexBegin) else
    delete(Content,IndexBegin+1,PEnd-SectionFirstLine);
  result := true; // Content was modified
end;

procedure ReplaceSection(SectionFirstLine: PUTF8Char;
  var Content: RawUTF8; const NewSectionContent: RawUTF8); overload;
var PEnd: PUTF8Char;
    IndexBegin: PtrInt;
begin
  if SectionFirstLine=nil then
    exit;
  // delete existing [Section] content
  PEnd := SectionFirstLine;
  while (PEnd<>nil) and (PEnd^<>'[') do
    GetNextLineBegin(PEnd,PEnd);
  IndexBegin := SectionFirstLine-pointer(Content);
  if PEnd=nil then
    SetLength(Content,IndexBegin) else
    delete(Content,IndexBegin+1,PEnd-SectionFirstLine);
  // insert section content
  insert(NewSectionContent,Content,IndexBegin+1);
end;

procedure ReplaceSection(var Content: RawUTF8; const SectionName,
  NewSectionContent: RawUTF8);
var UpperSection: array[byte] of AnsiChar;
    P: PUTF8Char;
begin
  P := pointer(Content);
  PWord(UpperCopy255(UpperSection,SectionName))^ := ord(']');
  if FindSectionFirstLine(P,UpperSection) then
    ReplaceSection(P,Content,NewSectionContent) else
    Content := Content+'['+SectionName+']'#13#10+NewSectionContent;
end;

function FindIniNameValueInteger(P: PUTF8Char; UpperName: PUTF8Char): integer;
begin
  result := GetInteger(pointer(FindIniNameValue(P,UpperName)));
end;

function FindIniEntry(const Content, Section, Name: RawUTF8): RawUTF8;
var P: PUTF8Char;
    UpperSection, UpperName: array[byte] of AnsiChar;
    // possible GPF if length(Section/Name)>255, but should const in code
begin
  result := '';
  P := pointer(Content);
  if P=nil then exit;
  // UpperName := UpperCase(Name)+'=';
  PWord(UpperCopy255(UpperName,Name))^ := ord('=');
  if Section='' then
     // find the Name= entry before any [Section]
    result := FindIniNameValue(P,UpperName) else begin
     // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(UpperSection,Section))^ := ord(']');
    if FindSectionFirstLine(P,UpperSection) then
      result := FindIniNameValue(P,UpperName);
  end;
end;

function FindWinAnsiIniEntry(const Content, Section,Name: RawUTF8): RawUTF8;
begin
  result := WinAnsiToUtf8(WinAnsiString(FindIniEntry(Content,Section,Name)));
end;

function FindIniEntryInteger(const Content,Section,Name: RawUTF8): integer;
begin
  result := GetInteger(pointer(FindIniEntry(Content,Section,Name)));
end;

function FindIniEntryFile(const FileName: TFileName; const Section,Name: RawUTF8): RawUTF8;
var Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  if Content='' then
    result := '' else
    result := FindIniEntry(Content,Section,Name);
end;

procedure UpdateIniEntry(var Content: RawUTF8; const Section,Name,Value: RawUTF8);
const CRLF = #13#10;
var P: PUTF8Char;
    PDeb: PUTF8Char;
    SectionFound: boolean;
    i, UpperNameLength: PtrInt;
    V: RawUTF8;
    UpperSection, UpperName: array[byte] of AnsiChar;
    // possible GPF if length(Section/Name)>255, but should be short const in code
label Sec;
begin
  PWord(UpperCopy255(UpperName,Name))^ := ord('=');
  UpperNameLength := length(Name)+1;
  V := Value+CRLF;
  P := pointer(Content);
  // 1. find Section, and try update within it
  if Section='' then
    goto Sec; // find the Name= entry before any [Section]
  SectionFound := false;
  PWord(UpperCopy255(UpperSection,Section))^ := ord(']');
  if FindSectionFirstLine(P,UpperSection) then begin 
Sec:SectionFound := true;
    while (P<>nil) and (P^<>'[') do begin
      PDeb := GetNextLineBegin(P,P); // since PDeb=P, we have PDeb<>nil
      while PDeb^=' ' do inc(PDeb);   // trim left ' '
      if IdemPChar(PDeb,UpperName) then begin
        // update Name=Value entry
        inc(PDeb,UpperNameLength);
        i := (PDeb-pointer(Content))+1;
        if (i=length(Value)) and CompareMem(PDeb,pointer(Value),i) then
          exit; // new Value is identical to the old one -> no change
        if P=nil then // avoid last line (P-PDeb) calculation error
          SetLength(Content,i-1) else
          delete(Content,i,P-PDeb); // delete old Value
        insert(V,Content,i); // set new value
        exit;
      end;
    end;
    // we reached next [Section] without having found Name=
   end;
  // 2. section or Name= entry not found: add Name=Value
  V := Name+'='+V;
  if not SectionFound then
    // create not existing [Section]
    V := '['+Section+(']'+CRLF)+V;
  // insert Name=Value at P^ (end of file or end of [Section])
  if P=nil then
    // insert at end of file
    Content := Content+V else begin
    // insert at end of [Section]
    i := (P-pointer(Content))+1;
    insert(V,Content,i);
  end;
end;

procedure UpdateIniEntryFile(const FileName: TFileName; const Section,Name,Value: RawUTF8);
var Content: RawUTF8;
begin
  Content := StringFromFile(FileName);
  UpdateIniEntry(Content,Section,Name,Value);
  FileFromString(Content,FileName);
end;


function StringFromFile(const FileName: TFileName): RawByteString;
var F: THandle;
    Size: integer;
begin
  result := '';
  if FileName='' then
    exit;
  F := FileOpen(FileName,fmOpenRead or fmShareDenyNone);
  if PtrInt(F)>=0 then begin
{$ifdef LINUX}
    Size := FileSeek(F,0,soFromEnd);
    FileSeek(F,0,soFromBeginning);
{$else}
    Size := GetFileSize(F, nil);
{$endif}
    SetLength(result,Size);
    if FileRead(F,pointer(Result)^,Size)<>Size then
      result := '';
    FileClose(F);
  end;
end;

function FileFromString(const Content: RawByteString; const FileName: TFileName;
  FlushOnDisk: boolean=false): boolean;
var F: THandle;
    L: integer;
begin
  result := false;
  F := FileCreate(FileName);
  if PtrInt(F)<0 then
    exit;
  if pointer(Content)<>nil then
    L := FileWrite(F,pointer(Content)^,length(Content)) else
    L := 0;
  result := (L=length(Content));
{$ifdef MSWINDOWS}
  if FlushOnDisk then
    FlushFileBuffers(F);
{$endif}
  FileClose(F);
end;

function ReadStringFromStream(S: TStream; MaxAllowedSize: integer): RawUTF8;
var L: integer;
begin
  result := '';
  L := 0;
  if (S.Read(L,4)<>4) or (L<=0) or (L>MaxAllowedSize) then
    exit;
  SetLength(Result,L);
  if S.Read(pointer(result)^,L)<>L then
    result := '';
end;

procedure WriteStringToStream(S: TStream; const Text: RawUTF8);
var L: integer;
begin
  L := length(Text);
  if L=0 then
    S.Write(L,4) else
    S.Write(pointer(PtrInt(Text)-4)^,L+4);
end;

function GetFileNameWithoutExt(const FileName: TFileName): TFileName;
var i, max: PtrInt;
begin
  i := length(FileName);
  max := i-4;
  while (i>0) and not(cardinal(FileName[i]) in [ord('\'),ord('/'),ord('.')])
    and (i>=max) do dec(i);
  if (i=0) or (FileName[i]<>'.') then
    result := FileName else
    SetString(result,PChar(pointer(FileName)),i-1);
end;

function GetFileNameExtIndex(const FileName, CSVExt: TFileName): integer;
var Ext: TFileName;
    P: PChar;
begin
  result := -1;
  P := pointer(CSVExt);
  Ext := ExtractFileExt(FileName);
  if (P=nil) or (Ext='') or (Ext[1]<>'.') then
    exit;
  delete(Ext,1,1);
  repeat
    inc(result);
    if SameText(GetNextItemString(P),Ext) then
      exit;
  until P=nil;
  result := -1;
end;

function FileSize(const FileName: TFileName): Int64;
begin
  try
    with TFileStream.Create(FileName,fmOpenRead or fmShareDenyNone) do
    try
      result := Seek(0,soFromEnd);
    finally
      Free;
    end;
  except
    on Exception do
      result := 0;
  end;
end;

function FileAgeToDateTime(const FileName: TFileName): TDateTime;
{$ifdef HASNEWFILEAGE}
begin
  if not FileAge(FileName,result) then
{$else}
var Age: integer;
begin
  Age := FileAge(FileName);
  if Age<>-1 then
    result := FileDateToDateTime(Age) else
{$endif}
    result := 0;
end;

{$ifdef DELPHI5OROLDER}
/// DirectoryExists returns a boolean value that indicates whether the
//  specified directory exists (and is actually a directory)
function DirectoryExists(const Directory: string): boolean;
var Code: Integer;
begin
  Code := GetFileAttributes(PChar(Directory));
  Result := (Code <> -1) and (FILE_ATTRIBUTE_DIRECTORY and Code <> 0);
end;

function GetModuleName(Module: HMODULE): TFileName;
var tmp: array[byte] of char;
begin
  SetString(Result,tmp,GetModuleFileName(Module,tmp,SizeOf(tmp)));
end;

function TryEncodeTime(Hour, Min, Sec, MSec: Word; var Time: TDateTime): Boolean;
begin
  result := false;
  if (Hour < 24) and (Min < 60) and (Sec < 60) and (MSec < 1000) then begin
    Time := (Hour * 3600000 + Min * 60000 + Sec * 1000 + MSec) / MSecsPerDay;
    result := true;
  end;
end;
{$endif}

{$ifdef FPC}
function Trim(const S: RawUTF8): RawUTF8;
var I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := '' else begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L-I+1);
  end;
end;
{$endif}

{$IFDEF PUREPASCAL}

{$IFDEF UNICODE}
function Trim(const S: RawUTF8): RawUTF8;
var I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and (S[I] <= ' ') do Inc(I);
  if I > L then
    Result := '' else begin
    while S[L] <= ' ' do Dec(L);
    Result := Copy(S, I, L-I+1);
  end;
end;

{$ifndef UNICODE}
function Pos(const substr, str: RawUTF8): Integer; overload;
begin // the RawByteString version is fast enough
  Result := PosEx(substr,str,1);
end;
{$endif}
{$ENDIF}
{$ENDIF}

function FindObjectEntry(const Content, Name: RawUTF8): RawUTF8;
var L: integer;
begin
  result := Trim(FindIniEntry(Content,'',Name+' ')); // 'Name = Value' format
  if (result<>'') and (result[1]='''') then begin
    L := length(result);
    if result[L]='''' then
      result := copy(result,2,L-2); // 'testDI6322.IAS' -> testDI6322.IAS
  end;
end;

function FindObjectEntryWithoutExt(const Content, Name: RawUTF8): RawUTF8;
begin
  result := RawUTF8(GetFileNameWithoutExt(
    ExtractFileName(TFileName(FindObjectEntry(Content,Name)))));
end;

function IntegerScanExists(P: PCardinalArray; Count: PtrInt; Value: cardinal): boolean;
{$ifdef PUREPASCAL}
var i: PtrInt; // very optimized code for speed
begin
  if P<>nil then begin
    result := true;
    for i := 1 to (Count shr 2) do   // 4 DWORD by loop - aligned read
      if (P^[0]=Value) or (P^[1]=Value) or
         (P^[2]=Value) or (P^[3]=Value) then
        exit else
        inc(PtrUInt(P),16);
    for i := 0 to (Count and 3)-1 do // last 0..3 DWORD
      if P^[i]=Value then
        exit;
  end;
  result := false;
end;
{$else}
asm // eax=P, edx=Count, Value=ecx
  test eax,eax
  jz @end // avoid GPF
  cmp edx,8
  jae @s1
  jmp dword ptr [edx*4+@Table]
  nop // align @Table
@Table:
  dd @z, @1, @2, @3, @4, @5, @6, @7
@s1: // fast search by 8 integers (pipelined instructions)
  sub edx,8
  cmp [eax],ecx;    je @ok
  cmp [eax+4],ecx;  je @ok
  cmp [eax+8],ecx;  je @ok
  cmp [eax+12],ecx; je @ok
  cmp [eax+16],ecx; je @ok
  cmp [eax+20],ecx; je @ok
  cmp [eax+24],ecx; je @ok
  cmp [eax+28],ecx; je @ok
  cmp edx,8
  lea eax,[eax+32] // preserve flags during 'cmp edx,8' computation
@s2:
  jae @s1
  jmp dword ptr [edx*4+@Table]
@7: cmp [eax+24],ecx; je @ok
@6: cmp [eax+20],ecx; je @ok
@5: cmp [eax+16],ecx; je @ok
@4: cmp [eax+12],ecx; je @ok
@3: cmp [eax+8],ecx;  je @ok
@2: cmp [eax+4],ecx;  je @ok
@1: cmp [eax],ecx;    je @ok
@z:
  xor eax,eax
@end:
  ret
@ok:
  mov al,1
end;
{$endif}

function IntegerScan(P: PCardinalArray; Count: PtrInt; Value: cardinal): PCardinal;
{$ifdef PUREPASCAL}
var i: PtrInt;
begin // very optimized code
  if P<>nil then begin
    for i := 1 to Count shr 2 do      // 4 DWORD by loop - aligned read
      if P^[0]<>Value then
      if P^[1]<>Value then
      if P^[2]<>Value then
      if P^[3]=Value then begin
        result := @P^[3];
        exit;
      end else
        inc(PtrUInt(P),16) else begin
        result := @P^[2];
        exit;
      end else begin
        result := @P^[1];
        exit;
      end else begin
        result := pointer(P);
        exit;
      end;
    for i := 0 to (Count and 3)-1 do  // last 0..3 DWORD
      if P^[i]=Value then begin
        result := @P^[i];
        exit;
      end;
  end;
  result := nil;
end;
{$else}
asm // eax=P, edx=Count, Value=ecx
       or eax,eax
       jz @ok0 // avoid GPF
       cmp edx,8
       jmp @s2
@ok0:  rep ret
@ok20: lea eax,[eax+20]; ret
@ok16: lea eax,[eax+16]; ret
@ok12: lea eax,[eax+12]; ret
@ok8:  lea eax,[eax+8]; ret
@ok4:  lea eax,[eax+4]; ret
@ok28: lea eax,[eax+28]; ret
@ok24: lea eax,[eax+24]; ret
       nop // align
@s1:   sub edx,8
       cmp [eax],ecx;    je @ok0
       cmp [eax+4],ecx;  je @ok4
       cmp [eax+8],ecx;  je @ok8
       cmp [eax+12],ecx; je @ok12
       cmp [eax+16],ecx; je @ok16
       cmp [eax+20],ecx; je @ok20
       cmp [eax+24],ecx; je @ok24
       cmp [eax+28],ecx; je @ok28
       cmp edx,8
       lea eax,[eax+32]  // preserve flags during 'cmp edx,8' computation
@s2:   jae @s1
       test edx,edx; jz @z
       cmp [eax],ecx;    je @ok0;  dec edx; jz @z
       cmp [eax+4],ecx;  je @ok4;  dec edx; jz @z
       cmp [eax+8],ecx;  je @ok8;  dec edx; jz @z
       cmp [eax+12],ecx; je @ok12; dec edx; jz @z
       cmp [eax+16],ecx; je @ok16; dec edx; jz @z
       cmp [eax+20],ecx; je @ok20; dec edx; jz @z
       cmp [eax+24],ecx; je @ok24
@z:    xor eax,eax // return nil if not found
end;
{$endif}

function AddInteger(var Values: TIntegerDynArray; Value: integer;
  NoDuplicates: boolean=false): boolean;
var n: PtrInt;
begin
  n := Length(Values);
  if NoDuplicates and IntegerScanExists(pointer(Values),n,Value) then begin
    result := false;
    exit;
  end;
  SetLength(Values,n+1);
  Values[n] := Value;
  result := true
end;

function AddInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: integer; NoDuplicates: boolean=false): boolean; overload;
begin
  if NoDuplicates and IntegerScanExists(pointer(Values),ValuesCount,Value) then begin
    result := false;
    exit;
  end;
  if ValuesCount=length(Values) then
    SetLength(Values,ValuesCount+256+ValuesCount shr 3);
  Values[ValuesCount] := Value;
  inc(ValuesCount);
  result := true
end;

procedure DeleteInteger(var Values: TIntegerDynArray; Index: PtrInt);
var n: PtrInt;
begin
  n := Length(Values);
  if PtrUInt(Index)>=PtrUInt(n) then
    exit; // wrong Index
  dec(n);
  if n>Index then
    move(Values[Index+1],Values[Index],(n-Index)*sizeof(Integer));
  SetLength(Values,n);
end;

procedure DeleteInteger(var Values: TIntegerDynArray; var ValuesCount: Integer; Index: PtrInt); overload;
var n: PtrInt;
begin
  n := ValuesCount;
  if PtrUInt(Index)>=PtrUInt(n) then
    exit; // wrong Index
  dec(n,Index+1);
  if n>0 then
    move(Values[Index+1],Values[Index],n*sizeof(Integer));
  dec(ValuesCount);
end;

procedure FillIncreasing(Values: PIntegerArray; StartValue, Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
    Values[i] := StartValue+i;
end;

procedure Int64ToUInt32(Values64: PInt64Array; Values32: PCardinalArray; Count: integer);
var i: integer;
begin
  for i := 0 to Count-1 do
   Values32[i] := Values64[i];
end;

procedure CSVToIntegerDynArray(CSV: PUTF8Char; var Result: TIntegerDynArray);
begin
  while CSV<>nil do begin
    SetLength(Result,length(Result)+1);
    Result[high(Result)] := GetNextItemCardinal(CSV);
  end;
end;

function IntegerDynArrayToCSV(const Values: TIntegerDynArray; ValuesCount: integer;
  const Prefix: RawUTF8=''; const Suffix: RawUTF8=''): RawUTF8;
type
  TInts16 = packed array[word] of string[15]; // shortstring are faster (no heap allocation)
var i, L, Len: PtrInt;
    tmp: array[0..15] of AnsiChar;
    ints: ^TInts16;
    P: PAnsiChar;
begin
  result := '';
  if ValuesCount=0 then
    exit;
  GetMem(ints,ValuesCount*sizeof(ints[0])); // getmem is faster than a dynamic array
  try
     // compute whole result length at once
    dec(ValuesCount);
    Len := length(Prefix)+length(Suffix);
    tmp[15] := ',';
    for i := 0 to ValuesCount do begin
      P := StrInt32(@tmp[15],Values[i]);
      L := @tmp[15]-P;
      if i<ValuesCount then
        inc(L); // append tmp[15]=','
      inc(Len,L);
      SetString(ints[i],P,L);
    end;
    // create result
    SetLength(result,Len);
    P := pointer(result);
    if Prefix<>'' then begin
      move(pointer(Prefix)^,P^,length(Prefix));
      inc(P,length(Prefix));
    end;
    for i := 0 to ValuesCount do begin
      Move(ints[i][1],P^,ord(ints[i][0]));
      inc(P,ord(ints[i][0]));
    end;
    if Suffix<>'' then
      move(pointer(Suffix)^,P^,length(Suffix));
  finally
    FreeMem(ints);
  end;
end;

function IntegerScanIndex(P: PCardinalArray; Count: PtrInt; Value: cardinal): PtrInt;
{$ifdef PUREPASCAL}
var i: PtrInt; // very optimized code for speed
begin
  if P<>nil then begin
    result := 0;
    for i := 1 to Count shr 2 do // 4 DWORD by loop - aligned read
      if P^[0]<>Value then
      if P^[1]<>Value then
      if P^[2]<>Value then
      if P^[3]<>Value then begin
        inc(PtrUInt(P),16);
        inc(result,4);
      end else begin
        inc(result,3);
        exit;
      end else begin
        inc(result,2);
        exit;
      end else begin
        inc(result,1);
        exit;
      end else
        exit;
    for i := 0 to (Count and 3)-1 do // last 0..3 DWORD
      if P^[i]=Value then
        exit else 
        inc(result);
  end;
  result := -1;
end;
{$else}
asm
    push eax
    call IntegerScan
    or eax,eax
    pop edx
    jz @z
    sub eax,edx
    shr eax,2
    ret
@z: mov eax,-1
end;
{$endif}

procedure QuickSortInteger(ID: PIntegerArray; L, R: PtrInt);
var I, J, P: PtrInt;
    pivot, Tmp: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      pivot := ID^[P];
      while ID[I]<pivot do Inc(I);
      while ID[J]>pivot do Dec(J);
      if I <= J then begin
        Tmp := ID[J]; ID[J] := ID[I]; ID[I] := Tmp;
        if P = I then P := J else if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortInteger(ID, L, J);
    L := I;
  until I >= R;
end;

procedure CopyAndSortInteger(Values: PIntegerArray; ValuesCount: integer;
  var Dest: TIntegerDynArray);  
begin
  if ValuesCount>length(Dest) then
    SetLength(Dest,ValuesCount);
  move(Values^[0],Dest[0],ValuesCount*sizeof(Integer));
  QuickSortInteger(pointer(Dest),0,ValuesCount-1);
end;

function FastFindIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;
var L: PtrInt;
    cmp: integer;
begin
  L := 0;
  if 0<=R then
  repeat
    result := (L + R) shr 1;
    cmp := P^[result]-Value;
    if cmp=0 then
      exit;
    if cmp<0 then
      L := result + 1 else
      R := result - 1;
  until (L > R);
  result := -1
end;

function FastLocateIntegerSorted(P: PIntegerArray; R: PtrInt; Value: integer): PtrInt;
var L,i: PtrInt;
   cmp: integer;
begin
  if R<0 then
    result := 0 else begin
    L := 0;
    result := -1; // return -1 if found
    repeat
      i := (L + R) shr 1;
      cmp := P^[i]-Value;
      if cmp=0 then
        exit;
      if cmp<0 then
        L := i + 1 else
        R := i - 1;
    until (L > R);
    while (i>=0) and (P^[i]>=Value) do dec(i);
    result := i+1; // return the index where to insert
  end;
end;

function AddSortedInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: integer; CoValues: PIntegerDynArray=nil): PtrInt;
begin
  result := FastLocateIntegerSorted(pointer(Values),ValuesCount-1,Value);
  if result>=0 then // if Value exists -> fails
    result := InsertInteger(Values,ValuesCount,Value,result,CoValues);
end;

function InsertInteger(var Values: TIntegerDynArray; var ValuesCount: integer;
  Value: Integer; Index: PtrInt; CoValues: PIntegerDynArray=nil): PtrInt;
var n: PtrInt;
begin
  result := Index;
  n := Length(Values);
  if ValuesCount=n then begin
    inc(n,256+n shr 3);
    SetLength(Values,n);
    if CoValues<>nil then
      SetLength(CoValues^,n);
  end;
  n := ValuesCount;
  if PtrUInt(result)<PtrUInt(n) then begin
    n := (n-result)*sizeof(Integer);
    move(Values[result],Values[result+1],n);
    if CoValues<>nil then
      move(CoValues^[result],CoValues^[result+1],n);
  end else
    result := n;
  Values[result] := Value;
  inc(ValuesCount);
end;

function GetInteger(P: PUTF8Char): PtrInt;
var c: PtrUInt;
    minus: boolean;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrInt(c);
      inc(P);
    until false;
  end;
  if minus then
    result := -result;
end;

function GetInteger(P: PUTF8Char; var err: integer): PtrInt;
var c: PtrUInt;
    minus: boolean;
begin
  if P=nil then begin
    result := 0;
    err := 1;
    exit;
  end else
    err := 0;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then begin
    err := 1;
    result := 0;
    exit;
  end else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then begin
        if byte(P^)<>0 then
          err := 1; // always return 1 as err code -> don't care about char index
        break;
      end else
        result := result*10+PtrInt(c);
      inc(P);
    until false;
  end;
  if minus then
    result := -result;
end;

function GetCardinalDef(P: PUTF8Char; Default: PtrUInt): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := Default;
    exit;
  end;
  P := GotoNextNotSpace(P);
  c := byte(P^)-48;
  if c>9 then
    result := Default else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrUInt(c);
      inc(P);
    until false;
  end;
end;

function GetCardinal(P: PUTF8Char): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+PtrUInt(c);
      inc(P);
    until false;
  end;
end;

function GetCardinalW(P: PWideChar): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  if ord(P^) in [1..32] then repeat inc(P) until not(ord(P^) in [1..32]);
  c := word(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := word(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
end;

{$ifdef CPU64}
procedure SetInt64(P: PUTF8Char; var result: Int64);
begin // PtrInt is already int64 -> call previous version
  result := GetInteger(P);
end;
{$else}
procedure SetInt64(P: PUTF8Char; var result: Int64);
var c: cardinal;
    minus: boolean;
begin
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat
    c := byte(P^)-48;
    if c>9 then
      break else
      Int64Rec(result).Lo := Int64Rec(result).Lo*10+c;
    inc(P);
    if Int64Rec(result).Lo>=high(cardinal)div 10 then begin
      repeat
        c := byte(P^)-48;
        if c>9 then
          break else
          result := result shl 3+result+result; // fast result := result*10
          inc(result,c);
        inc(P);
      until false;
      break;
    end;
  until false;
  if minus then
    result := -result;
end;
{$endif}

{$ifdef CPU64}
function GetInt64(P: PUTF8Char): Int64;
begin // PtrInt is already int64 -> call previous version
  result := GetInteger(P);
end;
{$else}
function GetInt64(P: PUTF8Char): Int64;
var c: cardinal;
    minus: boolean;
begin
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat
    c := byte(P^)-48;
    if c>9 then
      break else
      Int64Rec(result).Lo := Int64Rec(result).Lo*10+c;
    inc(P);
    if Int64Rec(result).Lo>=high(cardinal)div 10 then begin
      repeat
        c := byte(P^)-48;
        if c>9 then
          break else
          result := result shl 3+result+result; // fast result := result*10
          inc(result,c);
        inc(P);
      until false;
      break;
    end;
  until false;
  if minus then
    result := -result;
end;
{$endif}

function GetInt64(P: PUTF8Char; var err: integer): Int64; overload;
{$ifdef ENHANCEDRTL}
begin
  val(PAnsiChar(P),result,err);
end;
{$else}
{$ifdef PUREPASCAL}
var c: cardinal;
    minus: boolean;
begin
  err := 0;
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  Inc(err);
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat
    inc(err);
    if Byte(P^)=0 then begin
      err := 0; // conversion success without error
      Break;
    end;
    c := byte(P^)-48;
    if c>9 then
      break else
      {$ifdef CPU64}
      result := result*10;
      {$else}
      result := result shl 3+result+result;
      {$endif}
    inc(result,c);
    inc(P);
  until false;
  if minus then
    result := -result;
end;
{$else}
asm // enhanced John O'Harrow code
  test  eax,eax
  jz    @@Null
  push  ebx
  push  esi
  push  edi
  push  edx                 {Save Code Address}
  push  eax                 {Save String Pointer}
  mov   esi,eax             {String Pointer}
  xor   ebx,ebx             {Clear Valid Flag and Sign Flag}
  xor   eax,eax             {Clear Result}
  xor   edx,edx
  jmp   @@TrimEntry
@@Null:
  mov   [edx],eax
  inc   dword ptr [edx]               {Code = 1}
  xor   edx,edx             {Result = 0}
  ret
@@Trim:                     {Strip Leading Spaces}
  inc   esi
@@TrimEntry:
  movzx ecx,byte ptr [esi]
  cmp   cl,' '
  je    @@Trim
  cmp   cl,'0'
  jle   @@CheckFirstChar
@@CheckAlpha:
  test  cl,$87
  jz    @@CheckX            {May be 'x' or 'X'}
@@NumLoop:
  sub   ecx,'0'
  cmp   ecx,9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   eax,MaxInt/10-9     {(MaxInt div 10)-9}
  ja    @@LargeNum
  lea   eax,[eax*4+eax]
  lea   eax,[eax*2+ecx]     {Result = Result * 10 + Digit}
  inc   esi
  mov   bl,1                {Valid := True}
  movzx ecx,byte ptr [esi]
  jmp   @@NumLoop
@@LargeNum:
  mov   bh,cl               {Save Digit}
  add   eax,eax
  adc   edx,edx
  mov   ecx,eax
  mov   edi,edx             {edi:ecx = Result * 2}
  shld  edx,eax,2
  add   eax,eax
  add   eax,eax             {edx:eax = Result * 8}
  add   eax,ecx
  adc   edx,edi             {Result = Result * 10}
  movzx ecx,bh              {Restore Digit}
  add   eax,ecx             {Add Digit to Result}
  adc   edx,0
  inc   esi
  movzx ecx,byte ptr [esi]
  sub   ecx,'0'
  cmp   ecx,9
  ja    @@NumDone           {Not '0'..'9'}
  cmp   edx,$0ccccccc       {May be Out of Range?}
  jb    @@LargeNum
  ja    @@SetSign           {Out of Range}
  cmp   eax,$cccccccc
  jna   @@LargeNum          {Within Range}
  jmp   @@SetSign
@@NumDone:
  cmp   edx,$80000000       {Check for Overflow}
  jb    @@SetSign
  jne   @@Overflow
  test  eax,eax
  jnz   @@Overflow
  test  ebx,ebx             {Sign Flag}
  js    @@Setsign           {Result is Valid (-MaxInt64-1)}
@@Overflow:
  dec   esi
  mov   bl,0               {Valid := False}
  jmp   @@SetSign
@@CheckFirstChar:
  cmp   cl,'-'
  je    @@PlusMinus
  cmp   cl,'+'
  jne   @@SignSet
@@PlusMinus:                {Starts with '+' or '-'}
  mov   bl,'+'+1
  sub   ebx,ecx             {Set Sign Flag: '+' -> +1, '-' -> -1}
  inc   esi
  mov   bl,0                {Valid := False}
  movzx ecx,byte ptr [esi]           {Character after '+' or '-'}
@@SignSet:
  cmp   cl,'$'
  je    @@Hex               {Hexadecimal}
  cmp   cl,'0'
  jne   @@CheckAlpha        {May start with 'x' or 'X'}
  inc   esi
  mov   bl,1                {Assume Valid = True}
  movzx ecx,byte ptr [esi]           {Character after '0'}
  jmp   @@CheckAlpha        {May start with '0x' or '0X'}
@@CheckX:
  mov   bh,cl
  or    bh,$20              {'X' -> 'x'}
  cmp   bh,'x'
  jne   @@NumLoop
@@Hex:
  mov   bl,0                {Valid := False}
@@HexLoop:
  inc   esi
  movzx ecx,byte ptr [esi]
  cmp   cl,'a'
  jb    @@CheckNum
  sub   cl,'a'-'A'          {'a' > 'A'}
@@CheckNum:
  sub   cl,'0'
  cmp   cl,9
  jna   @@CheckHexRange     {'0'..'9'}
  sub   cl,'A'-'0'
  cmp   cl,5                {Valid Hex Character?}
  ja    @@NotHex            {No: Invalid}
  add   cl,10               {Yes: Adjust Digit}
@@CheckHexRange:
  cmp   edx,$10000000
  jae   @@SetSign          {Overflow}
  shld  edx,eax,4          {Result := Result * 16}
  shl   eax,4
  add   eax,ecx            {Add Digit}
  adc   edx,0
  mov   bl,1               {Valid := True}
  jmp   @@HexLoop
@@NotHex:
  add   cl,'A'-'0'         {Restore Char-'0'}
@@SetSign:
  mov   ch,bl              {Save Valid Flag}
  sar   ebx,31             {Set Each Bit to Top Bit (Sign Flag)}
  xor   eax,ebx            {Negate Result if Necessary}
  xor   edx,ebx
  sub   eax,ebx
  sbb   edx,ebx
  dec   ch                  {0 if Valid,-1 if Invalid}
  or    cl,ch               {If Invalid, Force CL = -1}
  cmp   cl,-'0'
  jne   @@Error             {Not Valid or Not End of String}
  xor   esi,esi             {Code := 0}
  pop   ebx                 {Dump String Pointer}
@@Finished:
  pop   ecx
  mov   [ecx],esi           {Set Error Code}
  pop   edi
  pop   esi
  pop   ebx
  ret
@@Error:
  inc   esi
  pop   ecx                 {String Pointer}
  sub   esi,ecx
  jmp   @@Finished
end;
{$endif}
{$endif}

function GetExtended(P: PUTF8Char): extended;
var err: integer;
begin
  result := GetExtended(P,err);
  if err<>0 then
    result := 0;
end;

function GetExtended(P: PUTF8Char; out err: integer): extended;
{$ifdef FPC}
begin
  val(string(P),result,err);
end;
{$else}
{$ifdef ENHANCEDRTL}
begin
  val(PAnsiChar(P),result,err);
end;
{$else}
{$ifdef PUREPASCAL}
begin
  val(string(P),result,err);
end;
{$else}
const
  Ten: Double = 10.0;
// faster ValExt_JOH_IA32_8_a implementation by John O'Harrow
// also avoid val() conversion into UnicodeString for Delphi 2009+
asm   // -> EAX Pointer to string
      //    EDX Pointer to code result
      // <- FST(0)  Result
  push  ebx              {Save Used Registers}
  push  esi
  push  edi
  mov   esi,eax         {String Pointer}
  push  eax             {Save for Error Condition}
  xor   ebx,ebx
  push  eax             {Allocate Local Storage for Loading FPU}
  test  esi,esi
  jz    @@Nil           {Nil String}
@@Trim:
  movzx ebx,byte ptr [esi]       {Strip Leading Spaces}
  inc   esi
  cmp   bl,' '
  je    @@Trim
  xor   ecx,ecx         {Clear Sign Flag}
{$IFDEF PIC}
  call  GetGOT
  fld   qword [eax.Ten] {Load 10 into FPU} {$ELSE}
  fld   qword [Ten]     {Load 10 into FPU}
{$ENDIF}
  xor   eax,eax         {Zero Number of Decimal Places}
  fldz                  {Zero Result in FPU}
  cmp   bl,'0'
  jl    @@CheckSign     {Check for Sign Character}
@@FirstDigit:
  xor   edi,edi         {Zero Exponent Value}
@@DigitLoop:
  sub   bl,'0'
  cmp   bl, 9
  ja    @@Fraction      {Non-Digit}
  mov   cl,1            {Set Digit Found Flag}
  mov   [esp],ebx       {Store for FPU Use}
  fmul  st(0), st(1)    {Multply by 10}
  fiadd dword ptr [esp] {Add Next Digit}
  movzx ebx,byte ptr [esi]       {Get Next Char}
  inc   esi
  test  bl,bl           {End Reached?}
  jnz   @@DigitLoop     {No, Get Next Digit}
  jmp   @@Finish        {Yes, Finished}
@@CheckSign:
  cmp   bl,'-'
  je    @@Minus
  cmp   bl,'+'
  je    @@SignSet
@@GetFirstDigit:
  test  bl, bl
  jz    @@Error         {No Digits Found}
  jmp   @@FirstDigit
@@Minus:
  mov   ch,1            {Set Sign Flag}
@@SignSet:
  movzx ebx,byte ptr [esi]       {Get Next Char}
  inc   esi
  jmp   @@GetFirstDigit
@@Fraction:
  cmp   bl,'.'-'0'
  jne   @@Exponent      {No Decimal Point}
  movzx ebx,byte ptr [esi]       {Get Next Char}
  test  bl, bl
  jz    @@DotEnd        {String Ends with '.'}
  inc   esi
@@FractionLoop:
  sub   bl,'0'
  cmp   bl, 9
  ja    @@Exponent       {Non-Digit}
  mov   [esp],ebx
  dec   eax              {-(Number of Decimal Places)}
  fmul  st(0), st(1)     {Multply by 10}
  fiadd dword ptr [esp]  {Add Next Digit}
  movzx ebx,byte ptr [esi]        {Get Next Char}
  inc   esi
  test  bl,bl            {End Reached?}
  jnz   @@FractionLoop   {No, Get Next Digit}
  jmp   @@Finish         {Yes, Finished (No Exponent)}
@@DotEnd:
  test  cl,cl            {Any Digits Found before '.'?}
  jnz   @@Finish         {Yes, Valid}
  jmp   @@Error          {No, Invalid}
@@Exponent:
  or    bl, $20
  cmp   bl,'e'-'0'
  jne   @@Error          {Not 'e' or 'E'}
@@GetExponent:
  movzx ebx,byte ptr [esi]        {Get Next Char}
  inc   esi
  mov   cl, 0            {Clear Exponent Sign Flag}
  cmp   bl,'-'
  je    @@MinusExp
  cmp   bl,'+'
  je    @@ExpSignSet
  jmp   @@ExpLoop
@@MinusExp:
  mov   cl,1            {Set Exponent Sign Flag}
@@ExpSignSet:
  movzx ebx,byte ptr [esi]       {Get Next Char}
  inc   esi
@@ExpLoop:
  sub   bl,'0'
  cmp   bl, 9
  ja    @@Error         {Non-Digit}
  lea   edi,[edi+edi*4] {Multiply by 10}
  add   edi,edi
  add   edi,ebx         {Add Next Digit}
  movzx ebx,byte ptr [esi]       {Get Next Char}
  inc   esi
  test  bl, bl           {End Reached?}
  jnz   @@ExpLoop        {No, Get Next Digit}
@@EndExp:
  test  cl, cl           {Positive Exponent?}
  jz    @@Finish         {Yes, Keep Exponent Value}
  neg   edi              {No, Negate Exponent Value}
@@Finish:
  add   eax,edi         {Exponent Value - Number of Decimal Places}
  mov   [edx],ebx       {Result Code = 0}
  jz    @@PowerDone     {No call to _Pow10 Needed}
  mov   edi,ecx         {Save Decimal Sign Flag}
  call  System.@Pow10          {Raise to Power of 10}
  mov   ecx,edi         {Restore Decimal Sign Flag}
@@PowerDone:
  test  ch, ch           {Decimal Sign Flag Set?}
  jnz   @@Negate         {Yes, Negate Value}
@@Success:
  add   esp, 8           {Dump Local Storage and String Pointer}
@@Exit:
  ffree st(1)            {Remove Ten Value from FPU}
  pop   edi              {Restore Used Registers}
  pop   esi
  pop   ebx
  ret                    {Finished}
@@Negate:
  fchs                   {Negate Result in FPU}
  jmp   @@Success
@@Nil:
  inc   esi              {Force Result Code = 1}
  fldz                   {Result Value = 0}
@@Error:
  pop   ebx              {Dump Local Storage}
  pop   eax              {String Pointer}
  sub   esi,eax          {Error Offset}
  mov   [edx],esi        {Set Result Code}
  test  ch, ch           {Decimal Sign Flag Set?}
  jz    @@Exit           {No, exit}
  fchs                   {Yes. Negate Result in FPU}
  jmp   @@Exit           {Exit Setting Result Code}
end;
{$endif}
{$endif}
{$endif}

function GetUTF8Char(P: PUTF8Char): PtrUInt;
begin
  if P<>nil then begin
    result := byte(P[0]);
    if result and $80<>0 then
      if result and $20=0 then // UTF-8 decode
        result := result shl 6+byte(P[1])-$00003080 else
        result := (result shl 6+byte(P[1]))shl 6+byte(P[2])-$000E2080;
  end else
    result := PtrUInt(P);
end;

function NextUTF8Char(P: PUTF8Char; out Next: PUTF8Char): PtrUInt;
begin
  if P<>nil then begin
    result := byte(P[0]);
    inc(P);
    if result and $80=0 then
      Next := P else
      if result and $20=0  then begin // UTF-8 decode
        result := result shl 6+byte(P[0])-$00003080;
        Next := P+1;
      end else begin
        result := (result shl 6+byte(P[0]))shl 6+byte(P[1])-$000E2080;
        Next := P+2;
      end;
  end else begin
    result := PtrUInt(P);
    Next := P;
  end;
end;

function ContainsUTF8(p, up: PUTF8Char): boolean;
var u: PByte;
begin
  if (p<>nil) and (up<>nil) and (up^<>#0) then begin
    result := true;
    repeat
      u := pointer(up);
      repeat
        if GetNextUTF8Upper(p)<>u^ then
          break else
          inc(u);
        if u^=0 then
          exit; // up^ was found inside p^
      until false;
      p := FindNextUTF8WordBegin(p);
    until p=nil;
  end;
  result := false;
end;

function IdemFileExt(p, extup: PUTF8Char): Boolean;
var ext: PUTF8Char;
begin
  if (p<>nil) and (extup<>nil) then begin
    ext := nil;
    repeat
      if p^='.' then
        ext := p; // get last '.' position from p into ext
      inc(p);
    until p^=#0;
    if ext<>nil then
      result := IdemPChar(ext,extup) else
      result := false;
  end else
    result := false;
end;

{$ifdef PUREPASCAL}
function IdemPChar(p, up: PUTF8Char): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    if up^<>NormToUpperAnsi7[p^] then
      exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;
{$else}
function IdemPChar(p, up: PUTF8Char): boolean;
// if the beginning of p^ is same as up^ (ignore case - up^ must be already Upper)
// eax=p edx=up
asm
  or eax,eax
  jz @e // P=nil -> false
  or edx,edx
  push ebx
  push esi
  jz @z // up=nil -> true
  mov esi,offset NormToUpperAnsi7
  xor ebx,ebx
  xor ecx,ecx
@1:
  mov cl,[edx] // cl=up^
  mov bl,[eax] // bl=p^
  test cl,cl
  mov bl,[ebx+esi] // bl=NormToUpperAnsi7[p^]
  jz @z // up^=#0 -> OK
  lea edx,[edx+1] // = inc edx without changing flags
  cmp bl,cl
  lea eax,[eax+1]
  je @1
  pop esi
  pop ebx
  xor eax,eax
@e:
  ret
@z:
  mov al,1 // up^=#0 -> OK
  pop esi
  pop ebx
end;
{$endif}

function IdemPCharArray(p: PUTF8Char; const upArray: array of PUTF8Char): integer;
var W: word;
begin
  if p<>nil then begin
    w := NormToUpperAnsi7Byte[ord(p[0])]+NormToUpperAnsi7Byte[ord(p[1])]shl 8;
    for result := 0 to high(upArray) do
      if (PWord(upArray[result])^=w) and IdemPChar(p+2,upArray[result]+2) then
        exit;
  end;
  result := -1;
end;

function IdemPCharU(p, up: PUTF8Char): boolean;
begin
  result := false;
  if (p=nil) or (up=nil) then
    exit;
  while up^<>#0 do begin
    if GetNextUTF8Upper(p)<>ord(up^) then
      exit;
    inc(up);
    inc(p);
  end;
  result := true;
end;

function UpperCopy255(dest: PAnsiChar; const source: RawUTF8): PAnsiChar;
var i, L: integer;
begin
  L := length(source);
  if L>0 then begin
    if L>250 then
      L := 250; // avoid buffer overflow
    result := dest+L;
    for i := 0 to L-1 do
      dest[i] := AnsiChar(NormToUpperAnsi7Byte[PByteArray(source)[i]]);
  end else
    result := dest;
end;

function UTF8UpperCopy(Dest, Source: PUTF8Char; SourceChars: Cardinal): PUTF8Char;
var c: cardinal;
    endSource, endSourceBy4: PUTF8Char;
label By1, By4; // ugly but faster
begin
  // first handle trailing 7 bit ASCII chars, by quad (Sha optimization)
  endSource := Source+SourceChars;
  endSourceBy4 := endSource-4;
  if (PtrUInt(Source) and 3=0) and (Source<=endSourceBy4) then
  repeat
By4:c := pCardinal(Source)^;
    if c and $80808080<>0 then
      goto By1; // break on first non ASCII quad
    inc(Source,4);
    Dest[0] := AnsiChar(NormToUpperByte[c and $ff]);
    Dest[1] := AnsiChar(NormToUpperByte[(c shr 8) and $ff]);
    Dest[2] := AnsiChar(NormToUpperByte[(c shr 16) and $ff]);
    Dest[3] := AnsiChar(NormToUpperByte[c shr 24]);
    inc(Dest,4);
  until Source>endSourceBy4;
  // generic loop, handling one UTF-8 code per iteration
  if Source<endSource then
  repeat
By1:c := byte(Source^); inc(Source);
    if ord(c) and $80=0 then begin
      Dest^ := AnsiChar(NormToUpperByte[c]);
      inc(Dest);
      if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
      if Source<endSource then continue else break;
    end else begin
      if Source>=endSource then break;
      if c and $20=0 then begin
        c := c shl 6+byte(Source^)-$00003080; inc(Source);
      end else begin
        c := c shl 6+byte(Source^); inc(Source);
        if Source>=endSource then break;
        c := c shl 6+byte(Source^)-$000E2080; inc(Source);
      end;
      if c and $ffffff00=0 then
        c := NormToUpperByte[c];
      if c<=$7f then begin
        Dest^ := AnsiChar(c);
        inc(Dest);
        if (PtrUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4 else
        if Source<EndSource then continue else break;
      end else
      if c>$7ff then begin
        Dest[0] := AnsiChar($E0 or (c shr 12));
        Dest[1] := AnsiChar($80 or ((c shr 6) and $3F));
        Dest[2] := AnsiChar($80 or (c and $3F));
        Inc(Dest,3);
        if (PtrUInt(Source) and 3=0) and (Source<=EndSourceBy4) then goto By4 else
        if Source<EndSource then continue else break;
      end else begin
        Dest[0] := AnsiChar($C0 or (c shr 6));
        Dest[1] := AnsiChar($80 or (c and $3F));
        Inc(Dest,2);
        if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
        if Source<endSource then continue else break;
      end;
      if (PtrUInt(Source) and 3=0) and (Source<EndSourceBy4) then goto By4 else
      if Source<endSource then continue else break;
    end;
  until false;
  result := Dest;
end;

function UTF8UpperCopy255(dest: PAnsiChar; const source: RawUTF8): PUTF8Char;
var L: integer;
begin
  L := length(source);
  if L>0 then begin
    if L>250 then
      L := 250; // avoid buffer overflow
    result := UTF8UpperCopy(pointer(dest),pointer(source),L);
  end else
    result := pointer(dest);
end;

function UpperCopy255W(dest: PAnsiChar; const source: SynUnicode): PAnsiChar;
var c: cardinal;
    i,L: integer;
begin
  L := length(source);
  if L>0 then begin
    if L>250 then
      L := 250; // avoid buffer overflow
    result := dest+L;
    for i := 0 to L-1 do begin
      c := PWordArray(source)[i];
      if c<255 then
        dest[i] := AnsiChar(NormToUpperAnsi7Byte[c]) else
        dest[i] := '?';
    end;
  end else
    result := dest;
end;

{$ifdef PUREPASCAL}
function UpperCopy(dest: PAnsiChar; const source: RawUTF8): PAnsiChar;
var s: PAnsiChar;
    c: cardinal;
begin
  s := pointer(source);
  if s<>nil then
    repeat
      c := ord(s^);
      if c=0 then
        break else
        dest^ := AnsiChar(NormToUpperAnsi7Byte[c]);
      inc(s);
      inc(dest);
    until false;
  result := dest;
end;
{$else}
function UpperCopy(dest: PAnsiChar; const source: RawUTF8): PAnsiChar;
asm // eax=dest source=edx
    or edx,edx
    jz @z
    push esi
    mov esi,offset NormToUpperAnsi7
    xor ecx,ecx
@1: mov cl,[edx]
    inc edx
    or cl,cl
    mov cl,[esi+ecx]
    jz @2
    mov [eax],cl
    inc eax
    jmp @1
@2: pop esi
@z:
end;
{$endif}

{$ifdef PUREPASCAL}
function UpperCopyShort(dest: PAnsiChar; const source: shortstring): PAnsiChar;
var i: PtrInt;
begin
  for i := 1 to ord(source[0]) do begin
    dest^ := AnsiChar(NormToUpperAnsi7Byte[ord(source[i])]);
    inc(dest);
  end;
  result := dest;
end;
{$else}
function UpperCopyShort(dest: PAnsiChar; const source: shortstring): PAnsiChar;
asm // eax=dest source=edx
    push esi
    push ebx
    movzx ebx,byte ptr [edx] // ebx = length(source)
    xor ecx,ecx
    or ebx,ebx
    mov esi,offset NormToUpperAnsi7
    jz @2 // source=''
    inc edx
@1: mov cl,[edx]
    inc edx
    dec ebx
    mov cl,[esi+ecx]
    mov [eax],cl
    lea eax,[eax+1]
    jnz @1
@2: pop ebx
    pop esi
@z:
end;
{$endif}

function GetNextLine(source: PUTF8Char; out next: PUTF8Char): RawUTF8;
begin
  next := source;
  if source=nil then begin
    result := '';
    exit;
  end;
  while source^ in ANSICHARNOT01310 do inc(source);
  SetString(result,PAnsiChar(next),source-next);
  if source^=#13 then inc(source);
  if source^=#10 then inc(source);
  if source^=#0 then
    next := nil else
    next := source;
end;

{$ifdef UNICODE}
function GetNextLineW(source: PWideChar; out next: PWideChar): string;
begin
  next := source;
  if source=nil then begin
    result := '';
    exit;
  end;
  while not (cardinal(source^) in [0,10,13]) do inc(source);
  SetString(result,PChar(next),source-next);
  if source^=#13 then inc(source);
  if source^=#10 then inc(source);
  if source^=#0 then
    next := nil else
    next := source;
end;

function FindIniNameValueW(P: PWideChar; UpperName: PUTF8Char): string;
var PDeb: PWideChar;
    L: PtrInt;
begin
  while (P<>nil) and (P^<>'[') do begin
    PDeb := P;
    while not (cardinal(P^) in [0,10,13]) do inc(P);
    while cardinal(P^) in [10,13] do inc(P);
    if P^=#0 then P := nil;
    if PDeb^=' ' then repeat inc(PDeb) until PDeb^<>' ';   // trim left ' '
    if IdemPCharW(PDeb,UpperName) then begin
      inc(PDeb,StrLen(UpperName));
      L := 0; while PDeb[L]>=' ' do inc(L); // get line length
      SetString(result,PDeb,L);
      exit;
    end;
  end;
  result := '';
end;

function FindIniEntryW(const Content: string; const Section, Name: RawUTF8): string;
var P: PWideChar;
    UpperSection, UpperName: array[byte] of AnsiChar;
    // possible GPF if length(Section/Name)>255, but should const in code
begin
  result := '';
  P := pointer(Content);
  if P=nil then exit;
  // UpperName := UpperCase(Name)+'=';
  PWord(UpperCopy255(UpperName,Name))^ := ord('=');
  if Section='' then
     // find the Name= entry before any [Section]
    result := FindIniNameValueW(P,UpperName) else begin
     // find the Name= entry in the specified [Section]
    PWord(UpperCopy255(UpperSection,Section))^ := ord(']');
    if FindSectionFirstLineW(P,UpperSection) then
      result := FindIniNameValueW(P,UpperName);
  end;
end;

{$endif}


function IdemPCharAndGetNextLine(var source: PUTF8Char; search: PUTF8Char): boolean;
{$ifdef PUREPASCAL}
begin
  if source=nil then
    result := false else begin
    result := IdemPChar(source,search);
    while source^ in ANSICHARNOT01310 do inc(source);
    while source^ in [#13,#10] do inc(source);
    if source^=#0 then
      source := nil;
  end;
end;
{$else}
asm // eax=source edx=search
    push eax       // save source var
    mov eax,[eax]  // eax=source
    or eax,eax
    jz @z
    push eax
    call IdemPChar
    pop ecx       // ecx=source
    push eax      // save result
@1: mov dl,[ecx]  // while not (source^ in [#0,#10,#13]) do inc(source);
    inc ecx
    cmp dl,13
    ja @1
    je @e
    or dl,dl
    jz @0
    cmp dl,10
    jne @1
    jmp @4
@e: cmp byte ptr [ecx],10 // jump #13#10
    jne @4
@3: inc ecx
@4: pop eax        // restore result
    pop edx        // restore source var
    mov [edx],ecx  // update source var
    ret
@0: xor ecx,ecx    // set source=nil
    jmp @4
@z: pop edx       // ignore source var, result := false
end;
{$endif}

function GetNextLineBegin(source: PUTF8Char; out next: PUTF8Char): PUTF8Char;
begin
  result := pointer(source);
  if source=nil then
    exit;
  while source^ in ANSICHARNOT01310 do inc(source);
  if source^=#13 then inc(source);
  if source^=#10 then inc(source);
  if source^=#0 then
    next := nil else
    next := source;
end;

function GetLineSize(P,PEnd: PUTF8Char): PtrUInt;
begin
  result := PtrUInt(P);
  if P<>nil then
    while (P<PEnd) and (P^ in ANSICHARNOT01310) do 
      inc(P);
  result := PtrUInt(P)-result;
end;

function GetNextItem(var P: PUTF8Char; Sep: AnsiChar= ','): RawUTF8;
var S: PUTF8Char;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
      P := S+1 else
      P := nil;
  end;
end;

function GetNextItemString(var P: PChar; Sep: Char= ','): string;
// this function will compile into AnsiString or UnicodeString, depending
// of the compiler version
var S: PChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(result,P,S-P);
    if S^<>#0 then
      P := S+1 else
      P := nil;
  end;
end;

function GetNextStringLineToRawUnicode(var P: PChar): RawUnicode;
var S: PChar;
begin
  if P=nil then
    result := '' else begin
    S := P;
    while S^>=' ' do
      inc(S);
    result := StringToRawUnicode(P,S-P);
    while (S^<>#0) and (S^<' ') do inc(S); // ignore e.g. #13 or #10
    if S^<>#0 then
      P := S else
      P := nil;
  end;
end;

procedure AppendCSVValues(const CSV: string; const Values: array of string;
  var Result: string; const AppendBefore: string=#13#10);
var Caption: string;
    i, bool: integer;
    P: PChar;
    first: Boolean;
begin
  P := pointer(CSV);
  if P=nil then
    exit;
  first := True;
  for i := 0 to high(Values) do begin
    Caption := GetNextItemString(P);
    if Values[i]<>'' then begin
      if first then begin
        Result := Result+#13#10;
        first := false;
      end else
        Result := Result+AppendBefore;
      bool := FindCSVIndex('0,-1',RawUTF8(Values[i]));
      Result := Result+Caption+': ';
      if bool<0 then
        Result := Result+Values[i] else
        Result := Result+GetCSVItemString(pointer(GetNextItemString(P)),bool,'/');
    end;
  end;
end;

procedure GetNextItemShortString(var P: PUTF8Char; out Dest: ShortString; Sep: AnsiChar= ',');
var S: PUTF8Char;
begin
  if P=nil then
    Dest[0] := #0 else begin
    S := P;
    while (S^<>#0) and (S^<>Sep) do
      inc(S);
    SetString(Dest,P,S-P);
    if S^<>#0 then
     P := S+1 else
     P := nil;
  end;
end;

function GetNextItemCardinal(var P: PUTF8Char; Sep: AnsiChar= ','): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  c := byte(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := byte(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
  while (P^<>#0) and (P^<>Sep) do // go to end of CSV item (ignore any decimal)
    inc(P);
  if P^=#0 then
    P := nil else
    inc(P);
end;

function CSVOfValue(const Value: RawUTF8; Count: cardinal; const Sep: RawUTF8=','): RawUTF8;
var ValueLen, SepLen: cardinal;
    i: cardinal;
    P: PAnsiChar;
begin // CSVOfValue('?',3)='?,?,?'
  if Count=0 then begin
    result := '';
    exit;
  end;
  ValueLen := length(Value);
  SepLen := Length(Sep);
  Setlength(result,ValueLen*Count+SepLen*pred(Count));
  P := pointer(result);
  i := 1;
  repeat
    move(Pointer(Value)^,P^,ValueLen);
    inc(P,ValueLen);
    if i=Count then
      break;
    move(Pointer(Sep)^,P^,SepLen);
    inc(P,SepLen);
    inc(i);
  until false;
  assert(P-pointer(result)=length(result));
end;

procedure SetBitCSV(var Bits; BitsCount: integer; var P: PUTF8Char);
var bit: cardinal;
begin
  while P<>nil do begin
    bit := GetNextItemCardinal(P)-1;
    if bit>=cardinal(BitsCount) then
      exit; // avoid GPF
    SetBit(Bits,bit);
  end;
end;

function GetBitCSV(const Bits; BitsCount: integer): RawUTF8;
var i: integer;
begin
  result := '';
  for i := 0 to BitsCount-1 do
    if GetBit(Bits,i) then
      result := result+UInt32ToUtf8(i+1)+',';
  if result<>'' then
    result := result+'0';
end;

function GetNextItemCardinalW(var P: PWideChar; Sep: WideChar= ','): PtrUInt;
var c: PtrUInt;
begin
  if P=nil then begin
    result := 0;
    exit;
  end;
  c := word(P^)-48;
  if c>9 then
    result := 0 else begin
    result := c;
    inc(P);
    repeat
      c := word(P^)-48;
      if c>9 then
        break else
        result := result*10+c;
      inc(P);
    until false;
  end;
  while (P^<>#0) and (P^<>Sep) do // go to end of CSV item (ignore any decimal)
    inc(P);
  if P^=#0 then
    P := nil else
    inc(P);
end;

function GetNextItemDouble(var P: PUTF8Char; Sep: AnsiChar= ','): double;
var tmp: RawUTF8;
    err: integer;
begin
  if P=nil then
    result := 0 else begin
    tmp := GetNextItem(P,Sep);
    result := GetExtended(pointer(tmp),err);
    if err<>0 then
      result := 0;
  end;
end;

function GetCSVItem(P: PUTF8Char; Index: PtrUInt; Sep: AnsiChar = ','): RawUTF8;
var i: PtrUInt;
begin
  if P=nil then
    result := '' else
    for i := 0 to Index do
      result := GetNextItem(P,Sep);
end;

function GetCSVItemString(P: PChar; Index: PtrUInt; Sep: Char = ','): string;
var i: PtrUInt;
begin
  if P=nil then
    result := '' else
    for i := 0 to Index do
      result := GetNextItemString(P,Sep);
end;

function FindCSVIndex(CSV: PUTF8Char; const Value: RawUTF8; Sep: AnsiChar = ',';
  CaseSensitive: boolean=true): integer;
var s: RawUTF8;
begin
  result := 0;
  while CSV<>nil do begin
    s := GetNextItem(CSV,Sep);
    if CaseSensitive then begin
      if s=Value then
        exit;
    end else
     if SameTextU(s,Value) then
       exit;
    inc(result);
  end;
  result := -1; // not found
end;

procedure CSVToRawUTF8DynArray(CSV: PUTF8Char; var Result: TRawUTF8DynArray; Sep: AnsiChar = ',');
var s: RawUTF8;
begin
  while CSV<>nil do begin
    s := GetNextItem(CSV,Sep);
    if s<>'' then begin
      SetLength(Result,length(Result)+1);
      Result[high(Result)] := s;
    end;
  end;
end;

function AddPrefixToCSV(CSV: PUTF8Char; const Prefix: RawUTF8; Sep: AnsiChar = ','): RawUTF8;
var s: RawUTF8;
begin
  result := GetNextItem(CSV,Sep);
  if result='' then
    exit;
  result := Prefix+result;
  while CSV<>nil do begin
    s := GetNextItem(CSV,Sep);
    if s<>'' then
      result := result+','+Prefix+s;
  end;
end;

function RawUTF8ArrayToCSV(const Values: array of RawUTF8; const Sep: RawUTF8 = ','): RawUTF8;
var i, len, seplen, L: Integer;
    P: PAnsiChar;
begin
  result := '';
  if high(Values)<0 then
    exit;
  seplen := length(Sep);
  len := seplen*high(Values);
  for i := 0 to high(Values) do
    inc(len,length(Values[i]));
  SetLength(result,len);
  P := pointer(result);
  i := 0;
  repeat
    L := length(Values[i]);
    if L>0 then begin
      move(pointer(Values[i])^,P^,L);
      inc(P,L);
    end;
    if i=high(Values) then
      Break;
    if seplen>0 then begin
      Move(pointer(Sep)^,P^,seplen);
      inc(P,seplen);
    end;
    inc(i);
  until false;
  Assert(P-pointer(result)=len);
end;

function RawUTF8ArrayToQuotedCSV(const Values: array of RawUTF8; const Sep: RawUTF8=',';
  Quote: AnsiChar=''''): RawUTF8;
var i: integer;
    tmp: TRawUTF8DynArray;
begin
  SetLength(tmp,length(Values));
  for i := 0 to High(Values) do
    tmp[i] := QuotedStr(Values[i],Quote);
  result := RawUTF8ArrayToCSV(tmp,Sep);
end;

function UrlEncode(const svar: RawUTF8): RawUTF8;
function Enc(s, p: PUTF8Char): PUTF8Char;
var c: PtrInt;
begin
  repeat
    c := ord(s^);
    case c of
    0: break;
    ord('0')..ord('9'),ord('a')..ord('z'),ord('A')..ord('Z'),
    ord('_'),ord('-'),ord('.'),ord('~'): begin
      // cf. rfc3986 2.3. Unreserved Characters
      p^ := AnsiChar(c);
      inc(p);
      inc(s);
      continue;
    end;
    ord(' '): p^ := '+';
    else begin
      p^ := '%'; inc(p);
      p^ := HexChars[c shr 4]; inc(p);
      p^ := HexChars[c and $F];
    end;
    end; // case c of
    inc(p);
    inc(s);
  until false;
  result := p;
end;
function Size(s: PUTF8Char): PtrInt;
begin
  result := 0;
  if s<>nil then 
  repeat
    case s^ of
      #0: exit;
      '0'..'9','a'..'z','A'..'Z','_','-','.','~',' ': begin
        inc(result);
        inc(s);
        continue;
      end;
      else inc(result,3);
    end;
    inc(s);
  until false;
end;
begin
  result := '';
  if pointer(svar)=nil then
    exit else
    SetLength(result,Size(pointer(svar))); // reserve exact memory count
  Enc(pointer(svar),pointer(result));
end;

function UrlDecode(const s: RawUTF8; i: PtrInt = 1; len: PtrInt = -1): RawUTF8;
var L: PtrInt;
    P: PUTF8Char;
begin
  result := '';
  if s='' then
    exit;
  L := PInteger(PtrInt(s)-4)^;
  if len<0 then
    len := L;
  if i>L then
    exit;
  dec(i);
  if len=i then
    exit;
  Setlength(result,len-i); // reserve enough space for result
  P := pointer(result);
  while i<len do begin
    case s[i+1] of
      #0: break; // reached end of s
      '%': if i+3>len then
        break else begin
        inc(i,2);
        P^ := AnsiChar(ConvertHexToBin[ord(s[i])] shl 4+ConvertHexToBin[ord(s[i+1])]);
      end;
      '+': P^  := ' ';
    else
      P^ := s[i+1];
    end; // case s[i] of
    inc(i);
    inc(P);
  end;
  Setlength(result,P-pointer(Result)); // fast with FastMM4/SynScaleMM (in-place realloc)
end;

function UrlDecode(U: PUTF8Char): RawUTF8;
var P,Dest: PUTF8Char;
    L: integer;
    tmp: array[byte] of AnsiChar;
begin
  L := StrLen(U);
  if L=0 then begin
    result := '';
    exit;
  end;
  if L>sizeof(tmp) then begin
    SetLength(result,L);
    Dest := pointer(result);
  end else
    Dest := @tmp;
  P := Dest;
  repeat
    case U^ of
      #0: break; // reached end of URI
      '%': if (U[1]=#0) or (U[2]=#0) then
        break else begin
        P^ := AnsiChar(ConvertHexToBin[ord(U[1])] shl 4+ConvertHexToBin[ord(U[2])]);
        inc(U,2);
      end;
      '+': P^  := ' ';
    else
      P^ := U^;
    end; // case s[i] of
    inc(U);
    inc(P);
  until false;
  if Dest=@tmp then
    SetString(result,tmp,P-Dest) else
    Setlength(result,P-Dest);
end;

function UrlDecodeValue(U, Upper: PUTF8Char; var Value: RawUTF8; Next: PPUTF8Char=nil): boolean;
var Beg, V: PUTF8Char;
    len, i: PtrInt;
begin
  // UrlDecodeValue('select=%2A&where=LastName%3D%27M%C3%B4net%27','SELECT=',V,@U)
  // -> U^='where=...' and V='*'
  result := false; // mark value not modified by default
  if U=nil then begin
    if Next<>nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U,Upper) then begin
    result := true;
    inc(U,StrLen(Upper));
    // compute resulting length of value
    Beg := U;
    len := 0;
    while not(U^ in [#0,'&']) do begin
      if U^='%' then
        if (U[1]=#0) or (U[2]=#0) then // avoid buffer overflow
          break else
          inc(U,3) else
        inc(U);
      inc(len);
    end;
    // decode value content
    SetLength(Value,len);
    V := pointer(Value);
    U := Beg;
    for i := 1 to len do
      if U^='%' then begin
        V^ := AnsiChar(ConvertHexToBin[ord(U[1])] shl 4+ConvertHexToBin[ord(U[2])]);
        inc(V);
        inc(U,3);
      end else begin
        if U^='+' then
          V^ := ' ' else
          V^ := U^;
        inc(V);
        inc(U);
      end;
  end;
  if Next=nil then
    exit;
  while not(U^ in [#0,'&']) do inc(U);
  if U^=#0 then
    Next^ := nil else
    Next^ := U+1; // jump '&'
end;

function UrlDecodeInteger(U, Upper: PUTF8Char; var Value: integer; Next: PPUTF8Char=nil): boolean;
var V: PtrInt;
    SignNeg: boolean;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U=nil then begin
    if Next<>nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U,Upper) then begin
    inc(U,StrLen(Upper));
    if U^='-' then begin
      SignNeg := True;
      Inc(U);
    end else
      SignNeg := false;
    if U^ in ['0'..'9'] then begin
      V := 0;
      repeat
        V := (V*10)+ord(U^)-48;
        inc(U);
      until not (U^ in ['0'..'9']);
      if SignNeg then
        Value := -V else
        Value := V;
      result := true;
    end;
  end;
  if Next=nil then
    exit;
  while not(U^ in [#0,'&']) do inc(U);
  if U^=#0 then
    Next^ := nil else
    Next^ := U+1; // jump '&'
end;

function UrlDecodeCardinal(U, Upper: PUTF8Char; var Value: Cardinal; Next: PPUTF8Char=nil): boolean;
var V: PtrInt;
begin
  // UrlDecodeInteger('offset=20&where=LastName%3D%27M%C3%B4net%27','OFFSET=',O,@Next)
  // -> Next^='where=...' and O=20
  result := false; // mark value not modified by default
  if U=nil then begin
    if Next<>nil then
      Next^ := U;
    exit;
  end;
  if IdemPChar(U,Upper) then begin
    inc(U,StrLen(Upper));
    if U^ in ['0'..'9'] then begin
      V := 0;
      repeat
        V := (V*10)+ord(U^)-48;
        inc(U);
      until not (U^ in ['0'..'9']);
      Value := V;
      result := true;
    end;
  end;
  if Next=nil then
    exit;
  while not(U^ in [#0,'&']) do inc(U);
  if U^=#0 then
    Next^ := nil else
    Next^ := U+1; // jump '&'
end;


function UrlDecodeInt64(U, Upper: PUTF8Char; var Value: Int64; Next: PPUTF8Char=nil): boolean;
var tmp: RawUTF8;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
    SetInt64(pointer(tmp),Value);
end;

function UrlDecodeExtended(U, Upper: PUTF8Char; var Value: Extended; Next: PPUTF8Char=nil): boolean;
var tmp: RawUTF8;
    err: integer;
begin
  result := UrlDecodeValue(U, Upper, tmp, Next);
  if result then
    Value := GetExtended(pointer(tmp),err);
end;

function UrlDecodeNeedParameters(U, CSVUpper: PUTF8Char): boolean;
var tmp: array[0..32] of AnsiChar;
    L: PtrInt;
    Beg: PUTF8Char;
// UrlDecodeNeedParameters('price=20.45&where=LastName%3D','PRICE,WHERE') will
// return TRUE
begin
  result := (CSVUpper=nil);
  if result then
    exit; // no parameter to check -> success
  repeat
    L := 0;
    while (CSVUpper^<>#0) and (CSVUpper^<>',') do begin
      tmp[L] := NormToUpper[CSVUpper^];
      if L=high(tmp) then
        exit else // invalid CSV parameter
        inc(L);
      inc(CSVUpper);
    end;
    if L=0 then
      exit; // invalid CSV parameter
    PWord(@tmp[L])^ := ord('=');
    Beg := U;
    repeat
      if IdemPChar(U,tmp) then
        break;
      while not(U^ in [#0,'&']) do inc(U);
      if U^=#0 then
        exit else // didn't find tmp in U
        inc(U); // Jump &
    until false;
    U := Beg;
    if CSVUpper^=#0 then
      Break else // no more parameter to check
      inc(CSVUpper); // jump &
  until false;
  result := true; // all parameters found
end;

function IsZero(P: pointer; Length: integer): boolean;
var i: integer;
begin
  result := false;
  for i := 1 to Length shr 4 do // 16 bytes (4 DWORD) by loop - aligned read
    if (PCardinalArray(P)^[0]<>0) or (PCardinalArray(P)^[1]<>0) or
       (PCardinalArray(P)^[2]<>0) or (PCardinalArray(P)^[3]<>0) then
      exit else
      inc(PtrUInt(P),16);
  for i := 1 to Length and 15 do
    if PByte(P)^<>0 then
      exit else
      inc(PtrUInt(P));
  result := true;
end;

{$WARNINGS OFF} // yes, we know there will be dead code below ;)
function IsZero(const Fields: TSQLFieldBits): boolean; overload;
begin
  if MAX_SQLFIELDS=64 then
    result := (PInt64(@Fields)^=0) else
  if MAX_SQLFields=128 then
    result := (PInt64Array(@Fields)^[0]=0) and (PInt64Array(@Fields)^[1]=0) else
  if MAX_SQLFields=192 then
    result := (PInt64Array(@Fields)^[0]=0) and (PInt64Array(@Fields)^[1]=0) and
      (PInt64Array(@Fields)^[2]=0) else
  if MAX_SQLFields=256 then
    result := (PInt64Array(@Fields)^[0]=0) and (PInt64Array(@Fields)^[1]=0) and
      (PInt64Array(@Fields)^[2]=0) and (PInt64Array(@Fields)^[3]=0) else
    result := IsZero(@Fields,sizeof(TSQLFieldBits))
end;
{$WARNINGS ON}

function Hash32(const Text: RawByteString): cardinal;
begin
  result := Hash32(pointer(Text),length(Text));
end;

function Hash32(Data: pointer; Len: integer): cardinal;
var s1,s2: cardinal;
    i: PtrInt;
begin
  if Data<>nil then begin
    s1 := 0;
    s2 := 0;
    for i := 1 to Len shr 4 do begin // 16 bytes (4 DWORD) by loop - aligned read
      inc(s1,PCardinalArray(Data)^[0]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[1]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[2]);
      inc(s2,s1);
      inc(s1,PCardinalArray(Data)^[3]);
      inc(s2,s1);
      inc(PtrUInt(Data),16);
    end;
    for i := 1 to (Len shr 2)and 3 do begin // 4 bytes (DWORD) by loop
      inc(s1,PCardinalArray(Data)^[0]);
      inc(s2,s1);
      inc(PtrUInt(Data),4);
    end;
    case Len and 3 of // remaining 0..3 bytes
    1: inc(s1,PByte(Data)^);
    2: inc(s1,PWord(Data)^);
    3: inc(s1,PWord(Data)^ or (PByteArray(Data)^[2] shl 16));
    end;
    inc(s2,s1);
    result := s1 xor (s2 shl 16);
  end else
    result := 0;
end;

function GetBit(const Bits; aIndex: PtrInt): boolean;
{$ifdef PUREPASCAL}
begin
{$ifdef CPU64}
  result := PInt64Array(@Bits)^[aIndex shr 6] and (Int64(1) shl (aIndex and 63)) <> 0;
{$else}
  result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
{$endif}
end;
{$else}
asm
  bt [eax],edx // use very fast i386 bit statement
  sbb eax,eax
  and eax,1
end;
{$endif}

procedure SetBit(var Bits; aIndex: PtrInt);
{$ifdef PUREPASCAL}
begin
{$ifdef CPU64}
  PInt64Array(@Bits)^[aIndex shr 6] := PInt64Array(@Bits)^[aIndex shr 6]
    or (Int64(1) shl (aIndex and 63));
{$else}
  PIntegerArray(@Bits)^[aIndex shr 5] := PIntegerArray(@Bits)^[aIndex shr 5]
    or (1 shl (aIndex and 31));
{$endif}
end;
{$else}
asm
  bts [eax],edx // use very fast i386 bit statement
end;
{$endif}

procedure UnSetBit(var Bits; aIndex: PtrInt);
{$ifdef PUREPASCAL} 
begin
  PIntegerArray(@Bits)^[aIndex shr 5] := PIntegerArray(@Bits)^[aIndex shr 5]
    and not (1 shl (aIndex and 31));
end;
{$else}
asm
  btr [eax],edx // use very fast i386 bit statement
end;
{$endif}

function GetBit64(const Bits; aIndex: PtrInt): boolean;
{$ifdef PUREPASCAL}
begin
  if PtrUInt(aIndex)>63 then
    result := false else
{$ifdef CPU64}
    result := PInt64(@Bits)^ and (Int64(1) shl (aIndex and 63)) <> 0;
{$else}
    result := PIntegerArray(@Bits)^[aIndex shr 5] and (1 shl (aIndex and 31)) <> 0;
{$endif}
end;
{$else}
asm
    cmp edx,64
    jae @z
    bt [eax],edx  // use very fast i386 bit statement
    sbb eax,eax
    and eax,1
    ret
@z: xor eax,eax
end;
{$endif}

function kr32(crc: cardinal; buf: PAnsiChar; len: cardinal): cardinal;
{$ifdef PUREPASCAL}
var i: integer;
begin
  for i := 0 to len-1 do
    crc := ord(buf[i])+crc*31;
  result := crc;
end;
{$else}
asm // eax=crc, edx=buf, ecx=len
    or ecx,ecx
    push edi
    push esi
    push ebx
    push ebp
    jz @z
    cmp ecx,4
    jb @s
@8: mov ebx,[edx] // unrolled version reading per DWORD
    lea edx,[edx+4]
    mov esi,eax
    movzx edi,bl
    movzx ebp,bh
    shr ebx,16
    shl eax,5
    sub eax,esi
    lea eax,[eax+edi]
    mov esi,eax
    shl eax,5
    sub eax,esi
    lea esi,[eax+ebp]
    lea eax,[eax+ebp]
    movzx edi,bl
    movzx ebx,bh
    shl eax,5
    sub eax,esi
    lea ebp,[eax+edi]
    lea eax,[eax+edi]
    shl eax,5
    sub eax,ebp
    cmp ecx,8
    lea eax,[eax+ebx]
    lea ecx,[ecx-4]
    jae @8
    or ecx,ecx
    jz @z
@s: mov esi,eax
@1: shl eax,5
    movzx ebx,byte ptr [edx]
    lea edx,[edx+1]
    sub eax,esi
    dec ecx
    lea esi,[eax+ebx]
    lea eax,[eax+ebx]
    jnz @1
@z: pop ebp
    pop ebx
    pop esi
    pop edi
end;
{$endif}

procedure SetBit64(var Bits: Int64; aIndex: PtrInt);
{$ifdef PUREPASCAL}
begin
  if PtrUInt(aIndex)<=63 then
{$ifdef CPU64}
    PInt64Array(@Bits)^[aIndex shr 6] := PInt64Array(@Bits)^[aIndex shr 6]
      or (Int64(1) shl (aIndex and 63));
{$else}
    PIntegerArray(@Bits)^[aIndex shr 5] := PIntegerArray(@Bits)^[aIndex shr 5]
      or (1 shl (aIndex and 31));
{$endif}
end;
{$else}
asm
  cmp edx,64
  jae @z
  bts [eax],edx  // use very fast i386 bit statement
@z:
end;
{$endif}

procedure UnSetBit64(var Bits: Int64; aIndex: PtrInt);
{$ifdef PUREPASCAL} 
begin
  if PtrUInt(aIndex)<=63 then
{$ifdef CPU64}
    PInt64Array(@Bits)^[aIndex shr 6] := PInt64Array(@Bits)^[aIndex shr 6]
      and not(Int64(1) shl (aIndex and 63));
{$else}
    PIntegerArray(@Bits)^[aIndex shr 5] := PIntegerArray(@Bits)^[aIndex shr 5]
      and not (1 shl (aIndex and 31));
{$endif}
end;
{$else}
asm
  cmp edx,64
  jae @z
  btr [eax],edx // use very fast i386 bit statement
@z:
end;
{$endif}

function GetBitsCount(const Bits; Count: PtrInt): integer;
{$ifdef PUREPASCAL}
begin
  result := 0;
  while Count>0 do begin
    dec(Count);
    if GetBit(Bits,Count) then
      inc(result);
  end;
end;
{$else}
asm
    xor ecx,ecx
@1: or edx,edx
    jz @n
    dec edx
    bt [eax],edx
    adc ecx,0
    jmp @1
@n: mov eax,ecx
end;
{$endif}

type TWordRec = packed record YDiv100, YMod100: byte; end;

{$ifdef PUREPASCAL}
function Div100(Y: PtrUInt): TWordRec;
{$ifdef HASINLINE}inline;{$endif}
begin
  result.YDiv100 := Y div 100;
  result.YMod100 := Y-(result.YDiv100*100); // * is always faster than div
end;
{$else}
function Div100(Y: word): TWordRec;
asm
  mov cl,100
  div cl // ah=remainder=Y mod 100, al=quotient=Year div 100
end;
{$endif}

{$ifdef DELPHI6OROLDER}
const
  HoursPerDay   = 24;
  MinsPerHour   = 60;
  SecsPerMin    = 60;
  MSecsPerSec   = 1000;
  MinsPerDay    = HoursPerDay * MinsPerHour;
  SecsPerDay    = MinsPerDay * SecsPerMin;
  MSecsPerDay   = SecsPerDay * MSecsPerSec;
{$endif}

function Iso8601ToDateTimePUTF8Char(P: PUTF8Char; L: integer): TDateTime;
begin
  Iso8601ToDateTimePUTF8CharVar(P,L,result);
end;

procedure Iso8601ToDateTimePUTF8CharVar(P: PUTF8Char; L: integer; var result: TDateTime);
var i: integer;
    B: cardinal;
    Y,M,D, H,MI,SS: cardinal;
// we expect 'YYYYMMDDThhmmss' format but we handle also 'YYYY-MM-DD hh:mm:ss'
begin
  result := 0;
  if P=nil then
    exit;
  if L=0 then
    L := StrLen(P);
  if L<4 then
    exit; // we need 'YYYY' at least
  if P[0]='T' then
    dec(P,8)
  else
  begin
    B := ConvertHexToBin[ord(P[0])]; // first digit
    if B>9 then
      exit
    else
      Y := B; // fast check '0'..'9'
    for i := 1 to 3 do
    begin
      B := ConvertHexToBin[ord(P[i])]; // 3 other digits
      if B>9 then
        exit
      else
        Y := Y*10+B;
    end;
    if P[4] in ['-','/'] then
    begin
      inc(P);
      dec(L);
    end; // allow YYYY-MM-DD
    D := 1;
    if L>=6 then
    begin // YYYYMM
      M := ord(P[4])*10+ord(P[5])-(48+480);
      if (M=0) or (M>12) then
        exit;
      if P[6] in ['-','/'] then
      begin
        inc(P);
        dec(L);
      end; // allow YYYY-MM-DD
      if L>=8 then
      begin // YYYYMMDD
        D := ord(P[6])*10+ord(P[7])-(48+480);
        if (D=0) or (D>MonthDays[true][M]) then
          exit; // worse is leap year=true
      end;
    end else
      M := 1;
    if M>2 then // inlined EncodeDate(Y,M,D)
      dec(M,3)
    else if M>0 then
    begin
      inc(M,9);
      dec(Y);
    end;
    if Y>3000 then
      exit; // avoid integer overflow e.g. if '0000' is an invalid date
    with Div100(Y) do
      result := (146097*YDiv100) shr 2 + (1461*YMod100) shr 2 +
            (153*M+2) div 5+D-693900;
    if (L<15) or not(P[8] in [' ','T']) then
      exit;
  end;
  H := ord(P[9])*10+ord(P[10])-(48+480);
  if P[11]=':' then inc(P); // allow hh:mm:ss
  MI := ord(P[11])*10+ord(P[12])-(48+480);
  if P[13]=':' then inc(P); // allow hh:mm:ss
  SS := ord(P[13])*10+ord(P[14])-(48+480);
  if (H<24) and (MI<60) and (SS<60) then // inlined EncodeTime()
    result := result + (H * (MinsPerHour * SecsPerMin * MSecsPerSec) +
             MI * (SecsPerMin * MSecsPerSec) + SS * MSecsPerSec) / MSecsPerDay;
end;

function Iso8601ToDateTime(const S: RawUTF8): TDateTime;
{$ifdef PUREPASCAL}
begin
  result := Iso8601ToDateTimePUTF8Char(pointer(S),length(S));
end;
{$else}
asm
  or eax,eax // if s='' -> p=nil -> will return 0, whatever L value is
  jz Iso8601ToDateTimePUTF8Char
  mov edx,[eax-4] // edx=L
@1: jmp Iso8601ToDateTimePUTF8Char
end;
{$endif}


/// Write a Date to P^ Ansi buffer
// - if Expanded is false, 'YYYYMMDD' date format is used
// - if Expanded is true, 'YYYY-MM-DD' date format is used
procedure DateToIso8601PChar(P: PUTF8Char; Expanded: boolean; Y,M,D: cardinal); overload;
begin
{$ifdef PUREPASCAL}
  PWord(P  )^ := TwoDigitLookupW[Y div 100];
  PWord(P+2)^ := TwoDigitLookupW[Y mod 100];
{$else}
  YearToPChar(Y,P);
{$endif}
  inc(P,4);
  if Expanded then begin
    P^ := '-';
    inc(P);
  end;
  pWord(P)^ := TwoDigitLookupW[M];
  inc(P,2);
  if Expanded then begin
    P^ := '-';
    inc(P);
  end;
  pWord(P)^ := TwoDigitLookupW[D];
end;

procedure TimeToIso8601PChar(P: PUTF8Char; Expanded: boolean; H,M,S: cardinal;
  FirstChar: AnsiChar = 'T'); overload;
// we use Thhmmss format
begin
  P^ := FirstChar;
  inc(P);
  pWord(P)^ := TwoDigitLookupW[H];
  inc(P,2);
  if Expanded then begin
    P^ := ':';
    inc(P);
  end;
  pWord(P)^ := TwoDigitLookupW[M];
  inc(P,2);
  if Expanded then begin
    P^ := ':';
    inc(P);
  end;
  pWord(P)^ := TwoDigitLookupW[S];
end;

procedure DateToIso8601PChar(Date: TDateTime; P: PUTF8Char; Expanded: boolean); overload;
// we use YYYYMMDD date format
var Y,M,D: word;
begin
  DecodeDate(Date,Y,M,D);
  DateToIso8601PChar(P,Expanded,Y,M,D);
end;

/// convert a date into 'YYYY-MM-DD' date format
function DateToIso8601Text(Date: TDateTime): RawUTF8;
begin
  SetLength(Result,10);
  DateToIso8601PChar(Date,pointer(Result),True);
end;

procedure TimeToIso8601PChar(Time: TDateTime; P: PUTF8Char; Expanded: boolean;
  FirstChar: AnsiChar = 'T'); overload;
// we use Thhmmss format
var H,M,S,MS: word;
begin
  DecodeTime(Time,H,M,S,MS);
  TimeToIso8601PChar(P,Expanded,H,M,S,FirstChar);
end;

function DateTimeToIso8601(D: TDateTime; Expanded: boolean;
  FirstChar: AnsiChar='T'): RawUTF8;
// we use YYYYMMDDThhmmss format
var tmp: array[0..31] of AnsiChar;
begin
  if Expanded then begin
    DateToIso8601PChar(D,tmp,true);
    TimeToIso8601PChar(D,@tmp[10],true,FirstChar);
    SetString(result,tmp,19);
  end else begin
    DateToIso8601PChar(D,tmp,false);
    TimeToIso8601PChar(D,@tmp[8],false,FirstChar);
    SetString(result,tmp,15);
  end;
end;

function DateToIso8601(Date: TDateTime; Expanded: boolean): RawUTF8;
// we use YYYYMMDDTdate format
begin
  SetString(result,nil,8+2*integer(Expanded));
{$ifdef UNICODE2} // not needed: SetLength() did already set the codepage
  PWord(PtrUInt(result)-12)^ := CP_UTF8; // use only SetLength() -> force set code page
{$endif}
  DateToIso8601PChar(Date,pointer(result),Expanded);
end;

/// basic Date conversion into ISO-8601
// - use 'YYYYMMDD' format if not Expanded
// - use 'YYYY-MM-DD' format if Expanded
function DateToIso8601(Y,M,D: cardinal; Expanded: boolean): RawUTF8; overload;
begin
  SetString(result,nil,8+2*integer(Expanded));
{$ifdef UNICODE2} // not needed: SetLength() did already set the codepage
  PWord(PtrUInt(result)-12)^ := CP_UTF8; // use only SetLength() -> force set code page
{$endif}
  DateToIso8601PChar(pointer(result),Expanded,Y,M,D);
end;

function TimeToIso8601(Time: TDateTime; Expanded: boolean; FirstChar: AnsiChar='T'): RawUTF8;
// we use Thhmmss format
begin
  SetString(result,nil,7+2*integer(Expanded));
{$ifdef UNICODE2} // not needed: SetLength() did already set the codepage
  PWord(PtrUInt(result)-12)^ := CP_UTF8; // use only SetLength() -> force set code page
{$endif}
  TimeToIso8601PChar(Time,pointer(result),Expanded,FirstChar);
end;

function DateTimeToIso8601Text(DT: TDateTime; FirstChar: AnsiChar): RawUTF8;
begin
  if DT=0 then
    result := '' else
    if frac(DT)=0 then
      result := DateToIso8601(DT,true) else
    if trunc(DT)=0 then
      result := TimeToIso8601(DT,true,FirstChar) else
      result := DateTimeToIso8601(DT,true,FirstChar);
end;

/// convert a Iso8601 encoded string into a "fake" second count
// - use internally for computation an abstract "year" of 16 months of 32 days
// of 32 hours of 64 minutes of 64 seconds
// - use this function only for fast comparaison between Iso8601 date/time
// - conversion is faster than Iso8601ToDateTime: use only binary integer math
function Iso8601ToSecondsPUTF8Char(P: PUTF8Char; L: integer; ContainsNoTime: PBoolean=nil): QWord;
// bits: S=0..5 M=6..11 H=12..16 D=17..21 M=22..25 Y=26..38
// i.e. S<64 M<64 H<32 D<32 M<16 Y<4096: power of 2 -> use fast shl for multiply
var V,B: PtrUInt;
    i: integer;
begin
  result := 0;
  if P=nil then
    exit;
  if L=0 then
    L := StrLen(P);
  if L<4 then
    exit; // we need 'YYYY' at least
  if P[0]='T' then
    dec(P,8) else begin // 'YYYY' -> year decode
    V := ConvertHexToBin[ord(P[0])]; if V>9 then exit;
    for i := 1 to 3 do begin
      B := ConvertHexToBin[ord(P[i])]; if B>9 then exit else V := V*10+B; end;
    result := Int64(V) shl 26;
    if P[4] in ['-','/'] then begin inc(P); dec(L); end; // allow YYYY-MM-DD
    if L>=6 then begin // YYYYMM
      V := ord(P[4])*10+ord(P[5])-(48+480+1); // Month 1..12 -> 0..11
      if V<=11 then
        inc(result,V shl 22) else begin
        result := 0;
        exit;
      end;
      if P[6] in ['-','/'] then begin inc(P); dec(L); end; // allow YYYY-MM-DD
      if L>=8 then begin  // YYYYMMDD
        V := ord(P[6])*10+ord(P[7])-(48+480+1); // Day 1..31 -> 0..30
        if V<=30 then
          inc(result,V shl 17) else begin
          result := 0;
          exit;
        end;
      end;
    end;
    if (L<15) or not(P[8] in [' ','T']) then begin
      if ContainsNoTime<>nil then
        ContainsNoTime^ := true;
      exit;
    end;
  end;
  if ContainsNoTime<>nil then
    ContainsNoTime^ := false;
  B := ord(P[9])*10+ord(P[10])-(48+480);
  if B<=23 then V := B shl 12 else exit;
  if P[11]=':' then inc(P); // allow hh:mm:ss
  B := ord(P[11])*10+ord(P[12])-(48+480);
  if B<=59 then inc(V,B shl 6) else exit;
  if P[13]=':' then inc(P); // allow hh:mm:ss
  B := ord(P[13])*10+ord(P[14])-(48+480);
  if B<=59 then inc(result,PtrUInt(V+B));
end;

function IsIso8601(P: PUTF8Char; L: integer): boolean;
begin
  result := Iso8601ToSecondsPUTF8Char(P,L)<>0;
end;

function Iso8601ToSeconds(const S: RawUTF8): Int64;
{$ifdef PUREPASCAL}
begin
  result := Iso8601ToSecondsPUTF8Char(pointer(S),length(S));
end;
{$else}
asm
    xor ecx,ecx  // ContainsNoTime=nil
    or eax,eax   // if s='' -> p=nil -> will return 0, whatever L value is
    jz Iso8601ToSecondsPUTF8Char
    mov edx,[eax-4] // edx=L
@1: jmp Iso8601ToSecondsPUTF8Char
end;
{$endif}


{ Iso8601 }

// bits: S=0..5 M=6..11 H=12..16 D=17..21 M=22..25 Y=26..38
// size: S=6 M=6  H=5  D=5  M=4  Y=12
// i.e. S<64 M<64 H<32 D<32 M<16 Y<4096: power of 2 -> use fast shl for multiply

procedure Iso8601.From(Y, M, D, HH, MM, SS: cardinal);
begin
  inc(HH,D shl 5+M shl 10+Y shl 14-(1 shl 5+1 shl 10));
  Value := SS+MM shl 6+Int64(HH) shl 12;
end;

procedure Iso8601.From(P: PUTF8Char; L: integer);
begin
  Value := Iso8601ToSecondsPUTF8Char(P,L);
end;

{$ifdef MSWINDOWS}
procedure Iso8601.Expand(out Date: TSystemTime);
begin
  Date.wYear := (Value shr (6+6+5+5+4)) and 4095;
  Date.wMonth := 1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15;
  Date.wDay := 1+(Int64Rec(Value).Lo shr (6+6+5)) and 31;
  Date.wDayOfWeek := 0;
  Date.wHour := (Int64Rec(Value).Lo shr (6+6)) and 31;
  Date.wMinute := (Int64Rec(Value).Lo shr 6) and 63;
  Date.wSecond := Int64Rec(Value).Lo and 63;
end;
{$endif}

procedure Iso8601.From(const S: RawUTF8);
begin
  Value := Iso8601ToSeconds(S);
end;

procedure Iso8601.From(FileDate: integer);
begin
{$ifdef MSWINDOWS}
  From(LongRec(FileDate).Hi shr 9 + 1980,
       LongRec(FileDate).Hi shr 5 and 15,
       LongRec(FileDate).Hi and 31,
       LongRec(FileDate).Lo shr 11,
       LongRec(FileDate).Lo shr 5 and 63,
       LongRec(FileDate).Lo and 31 shl 1);
{$else} // FileDate depends on the running OS
  From(FileDateToDateTime(FileDate));
{$endif}
end;

procedure Iso8601.From(DateTime: TDateTime; DateOnly: Boolean=false);
var HH,MM,SS,MS,Y,M,D: word;
    V: cardinal;
begin
  if DateOnly then
    HH := 0 else
    DecodeTime(DateTime,HH,MM,SS,MS);
  DecodeDate(DateTime,Y,M,D);
  V := HH+D shl 5+M shl 10+Y shl 14-(1 shl 5+1 shl 10);
  if DateOnly then
    Value := Int64(V) shl 12 else
    Value := SS+MM shl 6+Int64(V) shl 12;
end;

procedure Iso8601.FromNow;
{$ifdef MSWINDOWS}
var Now: TSystemTime;
    V: cardinal;
begin
  GetLocalTime(Now); // this API is fast enough for our purpose
  V := Now.wHour+Now.wDay shl 5+Now.wMonth shl 10+
    Now.wYear shl 14-(1 shl 5+1 shl 10);
  Value := Now.wSecond+Now.wMinute shl 6+Int64(V) shl 12;
end;
{$else} 
begin
  From(Now); // other OS: lets SysUtils.pas get the current time
end;
{$endif}

function Iso8601.ToTime: TDateTime;
begin
  if Value=0 then
    result := 0 else
    result := EncodeTime(
       (Int64Rec(Value).Lo shr (6+6)) and 31,
       (Int64Rec(Value).Lo shr 6) and 63,
        Int64Rec(Value).Lo and 63, 0);
end;

function Iso8601.ToDate: TDateTime;
begin
  if Value=0 then
    result := 0 else
    result := EncodeDate((Value shr (6+6+5+5+4)) and 4095,
                         1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15,
                         1+(Int64Rec(Value).Lo shr (6+6+5)) and 31);
end;

function Iso8601.ToDateTime: TDateTime;
begin
  if Value=0 then
    result := 0 else
    result := EncodeDate(
       (Value shr (6+6+5+5+4)) and 4095,
       1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15,
       1+(Int64Rec(Value).Lo shr (6+6+5)) and 31)+
      EncodeTime(
       (Int64Rec(Value).Lo shr (6+6)) and 31,
       (Int64Rec(Value).Lo shr 6) and 63,
       Int64Rec(Value).Lo and 63, 0);
end;

function Iso8601.Text(Dest: PUTF8Char; Expanded: boolean; FirstTimeChar: AnsiChar): integer;
begin
  if Value=0 then
    result := 0 else
  if Value and (1 shl (6+6+5)-1)=0 then begin
    // no Time: just convert date
    DateToIso8601PChar(Dest,Expanded,
      (Value shr (6+6+5+5+4)) and 4095,
      1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15,
      1+(Int64Rec(Value).Lo shr (6+6+5)) and 31);
    if Expanded then
      result := 10 else
      result := 8;
  end else
  if Value shr (6+6+5)=0 then begin
    // no Date: just convert time
    TimeToIso8601PChar(Dest,Expanded,
      (Int64Rec(Value).Lo shr (6+6)) and 31,
      (Int64Rec(Value).Lo shr 6) and 63,
       Int64Rec(Value).Lo and 63, FirstTimeChar);
    if Expanded then
      result := 9 else
      result := 7;
  end else begin
    // convert time and date
    DateToIso8601PChar(Dest,Expanded,
      (Value shr (6+6+5+5+4)) and 4095,
      1+(Int64Rec(Value).Lo shr (6+6+5+5)) and 15,
      1+(Int64Rec(Value).Lo shr (6+6+5)) and 31);
    if Expanded then
      inc(Dest,10) else
      inc(Dest,8);
    TimeToIso8601PChar(Dest,Expanded,
      (Int64Rec(Value).Lo shr (6+6)) and 31,
      (Int64Rec(Value).Lo shr 6) and 63,
       Int64Rec(Value).Lo and 63, FirstTimeChar);
    if Expanded then
      result := 15+4 else
      result := 15;
  end;
end;

function Iso8601.Text(Expanded: boolean; FirstTimeChar: AnsiChar = 'T'): RawUTF8;
var tmp: array[0..31] of AnsiChar;
begin
  if Value=0 then
    result := '' else
    SetString(result,tmp,Text(tmp,Expanded,FirstTimeChar));
end;

function Iso8601Now: Int64;
begin
  PIso8601(@result)^.FromNow;
end;

function NowToString(Expanded: boolean=true; FirstTimeChar: AnsiChar = ' '): RawUTF8;
var I: Iso8601;
begin
  I.FromNow;
  result := I.Text(Expanded,FirstTimeChar);
end;

function TimeToString: RawUTF8;
var I: Iso8601;
begin
  I.FromNow;
  I.Value := I.Value and (1 shl (6+6+5)-1); // keep only time
  result := I.Text(true,' ');
end;

function Iso8601FromFile(const FileName: TFileName): Int64;
var Date: TDateTime;
begin
  Date := FileAgeToDateTime(FileName);
  if Date=0 then
    result := 0
  else
    PIso8601(@result)^.From(Date);
end;

function Iso8601FromDateTime(DateTime: TDateTime): Int64;
begin
  PIso8601(@result)^.From(DateTime);
end;


procedure AppendToTextFile(aLine: RawUTF8; const aFileName: TFileName);
var F: THandle;
    Old: TFileName;
    Date: array[1..22] of AnsiChar;
    i: integer;
{$ifdef MSWINDOWS}
    Now: TSystemTime; {$else}
    D: TDateTime;     {$endif}
begin
  if aFileName='' then
    exit;
  F := FileOpen(aFileName,fmOpenWrite);
  if PtrInt(F)<0 then begin
    F := FileCreate(aFileName);
    if PtrInt(F)<0 then
      exit;
  end;
   // append to end of file
  if FileSeek(F,0,soFromEnd)>MAXLOGSIZE then begin
    // rotate log file if too big
    FileClose(F);
    Old := aFileName+'.bak'; // '.log.bak'
    DeleteFile(Old);  // rotate once
    RenameFile(aFileName,Old);
    F := FileCreate(aFileName);
    if PtrInt(F)<0 then
      exit;
  end;
  PWord(@Date)^ := 13+10 shl 8; // first go to next line
{$ifdef MSWINDOWS}
  GetLocalTime(Now); // windows dedicated function
  DateToIso8601PChar(@Date[3],true,Now.wYear,Now.wMonth,Now.wDay);
  TimeToIso8601PChar(@Date[13],true,Now.wHour,Now.wMinute,Now.wSecond,' ');
{$else}
  D := Now; // cross platform version
  DateToIso8601PChar(D,@Date[3],true);
  TimeToIso8601PChar(D,@Date[13],true);
{$endif}
  Date[22] := ' ';
  FileWrite(F,Date,sizeof(Date));
  for i := 1 to length(aLine) do
    if aLine[i]<' ' then
      aLine[i] := ' '; // avoid line feed in text log file
  FileWrite(F,pointer(aLine)^,length(aLine));
  FileClose(F);
end;

procedure LogToTextFile(Msg: RawUTF8);
begin
  if Msg='' then begin
    Msg := StringToUTF8(SysErrorMessage(GetLastError));
    if Msg='' then
      exit;
  end;
  AppendToTextFile(Msg,ChangeFileExt(paramstr(0),'.log'));
end;



{$ifndef PUREPASCAl}
function GetEnumName(aTypeInfo: pointer; aIndex: integer): PShortString;
asm // get enumerate name from RTTI
    or edx,edx
    movzx ecx,byte ptr [eax+1] // +1=TTypeInfo.Name
    mov eax,[eax+ecx+1+9+1] //BaseType
    mov eax,[eax]
    movzx ecx,byte ptr [eax+1]
    lea eax,[eax+ecx+1+9+4+1] // eax=EnumType.BaseType^.EnumType.NameList
    jz @0
@1: movzx ecx,byte ptr [eax]
    dec edx
    lea eax,[eax+ecx+1] // next short string
    jnz @1
@0:
end;
{$endif}

function StrCurr64(P: PAnsiChar; const Value: Int64): PAnsiChar;
var c, c10: Int64;
    c64: Int64Rec absolute c;
    Lo: cardinal;
begin
  if Value=0 then begin
    result := P-1;
    result^ := '0';
    exit;
  end;
  if Value<0 then
    c := -Value else
    c := Value;
  if (c64.Hi=0) and (c64.Lo<10000) then begin
    Lo := c64.Lo; // only decimals
    result := P;
  end else begin
    Lo := 10000;
    result := P-1; // reserve space to insert '.'
  end;
  repeat
    if c64.Hi=0 then begin
      result := StrUInt32(result,c64.Lo);
      break;
    end;
    c10 := c div 100;   // one div by two digits
    dec(c,c10*100);     // fast c := c mod 100
    dec(result,2);
    PWord(result)^ := TwoDigitLookupW[c];
    c := c10;
    if c10=0 then break;
  until false;
  if Lo<10000 then begin
    // only decimals -> append left '0.' to '0.000'
    case Lo of
    1..9: begin // append left '0.000'
      dec(result);
      result^ := '0';
      dec(result,2);
      PWord(result)^ := ord('0')+ord('0')shl 8;
    end;
    10..99: begin // append left '0.00'
      dec(result,2);
      PWord(result)^ := ord('0')+ord('0')shl 8;
    end;
    100..999: begin // append left '0.0'
      dec(result);
      result^ := '0';
    end;
    end;
    dec(result,2);
    PWord(result)^ := ord('0')+ord('.')shl 8;
  end else begin
    // insert '.' just before last 4 decimals
    P[-1] := P[-2];
    P[-2] := P[-3];
    P[-3] := P[-4];
    P[-4] := P[-5];
    P[-5] := '.';
  end;
  if Value<0 then begin
    dec(result);
    result^ := '-';
  end;
end;

function Curr64ToStr(Value: Int64): RawUTF8;
var tmp: array[0..31] of AnsiChar;
    P: PAnsiChar;
    Decim, L: Cardinal;
begin
  P := StrCurr64(@tmp[31],Value);
  L := @tmp[31]-P;
  if L>4 then begin
    Decim := PCardinal(P+L-4)^; // 4 last digits = 4 decimals
    if Decim=ord('0')+ord('0')shl 8+ord('0')shl 16+ord('0')shl 24 then
      dec(L,5) else // no decimal
    if Decim and $ffff0000=ord('0')shl 16+ord('0')shl 24 then
      dec(L,2); // 2 decimals
  end;
  SetString(result,P,L);
end;

function Curr64ToPChar(Value: Int64; Dest: PUTF8Char): PtrInt;
var tmp: array[0..31] of AnsiChar;
    P: PAnsiChar;
    Decim: Cardinal;
begin
  P := StrCurr64(@tmp[31],Value);
  result := @tmp[31]-P;
  move(P^,Dest^,result);
  if result<5 then
    exit;
  Decim := PCardinal(P+result-4)^; // 4 last digits = 4 decimals
  if Decim=ord('0')+ord('0')shl 8+ord('0')shl 16+ord('0')shl 24 then
    dec(result,5) else // no decimal
  if Decim and $ffff0000=ord('0')shl 16+ord('0')shl 24 then
    dec(result,2); // 2 decimals
end;

function StrToCurr64(P: PUTF8Char; NoDecimal: PBoolean=nil): Int64;
var c: cardinal;
    minus: boolean;
    Dec: cardinal;
begin
  result := 0;
  if P=nil then
    exit;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if P^='-' then begin
    minus := true;
    repeat inc(P) until P^<>' ';
  end else begin
    minus := false;
    if P^='+' then
      repeat inc(P) until P^<>' ';
  end;
  if P^='.' then begin // '.5' -> 500
    Dec := 2;
    inc(P);
  end else
    Dec := 0;
  c := byte(P^)-48;
  if c>9 then
    exit;
  Int64Rec(result).Lo := c;
  inc(P);
  repeat
    if P^<>'.' then begin
      c := byte(P^)-48;
      if c>9 then
        break;
      {$ifdef CPU64}
      result := result*10;
      {$else}
      result := result shl 3+result+result;
      {$endif}
      inc(result,c);
      inc(P);
      if Dec<>0 then begin
        inc(Dec);
        if Dec<5 then continue else break;
      end;
    end else begin
      inc(Dec);
      inc(P);
    end;
  until false;
  if NoDecimal<>nil then
    if Dec=0 then begin
      NoDecimal^ := true;
      if minus then
        result := -result;
      exit;
    end else
      NoDecimal^ := false;
  if Dec<>5 then // Dec=5 most of the time
  case Dec of
  0,1: result := result*10000;
  {$ifdef CPU64}
  2: result := result*1000;
  3: result := result*100;
  4: result := result*10;
  {$else}
  2: result := result shl 10-result shl 4-result shl 3;
  3: result := result shl 6+result shl 5+result shl 2;
  4: result := result shl 3+result+result;
  {$endif}
  end;
  if minus then
    result := -result;
end;

function StrToCurrency(P: PUTF8Char): currency;
begin
  PInt64(@result)^ := StrToCurr64(P,nil);
end;

function RoundTo2Digits(Value: Currency): Currency;
var V64: Int64 absolute Value; // to avoid any floating-point precision issues
    Spare: integer;
begin
  Spare := V64 mod 100;
  if Spare<>0 then
    dec(V64,Spare);
  result := Value;
end;

function TrimLeftLowerCase(V: PShortString): RawUTF8;
{$ifdef NODELPHIASM}
var P: PAnsiChar;
    L: integer;
begin
  L := length(V^);
  P := @V^[1];
  while (L>0) and (P^ in ['a'..'z']) do begin
    inc(P);
    dec(L);
  end;
  if L=0 then
    result := V^ else
    SetString(result,P,L);
end;
{$else}
asm // eax=V
    xor cl,cl
    push edx // save result RawUTF8
    or eax,eax
    jz @2 // avoid GPF
    lea edx,eax+1
    mov cl,[eax]
@1: mov ch,[edx] // edx=source cl=length
    sub ch,'a'
    sub ch,'z'-'a'
    ja @2 // not a lower char -> create a result string starting at edx
    inc edx
    dec cl
    jnz @1
    mov cl,[eax]
    lea edx,eax+1  // no UpperCase -> retrieve full text (result := V^)
@2: pop eax
    movzx ecx,cl
{$ifdef UNICODE}
    push CP_UTF8 // UTF-8 code page for Delphi 2009+ + call below, not jump
    call System.@LStrFromPCharLen // eax=Dest edx=Source ecx=Length
    ret // we need a call just above for right push CP_UTF8 retrieval
{$else}
    jmp System.@LStrFromPCharLen // eax=dest edx=source ecx=length(source)
{$endif}
end;
{$endif}

function UnCamelCase(const S: RawUTF8): RawUTF8; overload;
begin
  result := '';
  if S='' then
    exit;
  SetLength(result,PInteger(PtrInt(S)-4)^*2); // max length
{$ifdef UNICODE2} // not needed: SetLength() did already set the codepage
  PWord(PtrInt(result)-12)^ := CP_UTF8; // use only SetLength() -> force set code page
{$endif}
  SetLength(result,UnCamelCase(pointer(result),pointer(S)));
end;

function UnCamelCase(D, P: PUTF8Char): integer; overload;
var Space, SpaceBeg, DBeg: PUTF8Char;
label Next;
begin
  Space := D;
  DBeg := D;
  SpaceBeg := D;
  if (D<>nil) and (P<>nil) then // avoid GPF
  repeat
    repeat
      D^ := P^;
      inc(P);
      inc(D);
    until not (P^ in ['A'..'Z']);
    if P^=#0 then break; // no lowercase conversion of last fully uppercased word
    while P^ in ['a'..'z','0'..'9'] do begin
      D^  := P^;
      inc(D);
      inc(P);
    end;
    if P^='_' then
    if P[1]='_' then begin
      D^ := ':';
      inc(P);
      inc(D);
      goto Next;
    end else begin
      PWord(D)^ := ord(' ')+ord('-')shl 8;
      inc(D,2);
Next: if Space=SpaceBeg then
        SpaceBeg := D+1;
      inc(P);
      Space := D+1;
    end else
      Space := D;
    if P^=#0 then break;
    D^ := ' ';
    inc(D);
  until false;
  while Space>SpaceBeg do begin
    if (Space^ in ['A'..'Z']) then
      inc(Space^,32); // lowercase conversion of not last fully uppercased word
    dec(Space);
  end;
  result := D-DBeg;
end;

procedure GetCaptionFromPCharLen(P: PUTF8Char; out result: string);
var Temp: array[byte] of AnsiChar;
begin // "out result" parameter definition already made result := ''
  if P=nil then
    exit;
{$ifdef UNICODE}
  // property and enumeration names are UTF-8 encoded with Delphi 2009+
  result := UTF8DecodeToUnicodeString(Temp,UnCamelCase(@Temp,P));
{$else}
  SetString(result,Temp,UnCamelCase(@Temp,P));
{$endif}
{$ifndef LVCL} // LVCL system.pas doesn't implement LoadResStringTranslate()
  if Assigned(LoadResStringTranslate) then
    LoadResStringTranslate(string(result));
{$endif}
end;

function GetDisplayNameFromClass(C: TClass): RawUTF8;
var DelphiName: ShortString;
    TrimLeft: integer;
begin
  result := '';
  if C=nil then
    exit;
  // new TObject.ClassName is UnicodeString (since Delphi 20009) -> inline code
  // with vmtClassName = UTF-8 encoded text stored in a shortstring = -44
  DelphiName := PShortString(PPointer(PtrInt(C)+vmtClassName)^)^;
  TrimLeft := 0;
  if DelphiName[0]>#4 then
    case pInteger(@DelphiName[1])^ and $DFDFDFDF of
      // fast case-insensitive compare
      ord('T')+ord('S')shl 8+ord('Q')shl 16+ord('L')shl 24:
        if (DelphiName[0]<=#10) or
         (pInteger(@DelphiName[5])^ and $DFDFDFDF<> // fast case-insensitive compare
           ord('R')+ord('E')shl 8+ord('C')shl 16+ord('O')shl 24) or
         (pWord(@DelphiName[9])^ and $DFDF<>ord('R')+ord('D')shl 8) then
        TrimLeft := 4 else
        TrimLeft := 10;
      ord('T')+ord('S')shl 8+ord('Y')shl 16+ord('N')shl 24:
        TrimLeft := 4;
    end;
  if (Trimleft=0) and (DelphiName[1]='T') then
    Trimleft := 1;
  SetString(result,PAnsiChar(@DelphiName[TrimLeft+1]),ord(DelphiName[0])-TrimLeft);
end;

function GetCaptionFromClass(C: TClass): string;
var tmp: RawUTF8;
    P: PUTF8Char;
begin
  if C=nil then
    result := '' else begin
    tmp := RawUTF8(C.ClassName);
    P := pointer(tmp);
    if IdemPChar(P,'TSQL') or IdemPChar(P,'TSYN') then
      inc(P,4) else
    if P^='T' then
       inc(P);
    GetCaptionFromPCharLen(P,result);
  end;
end;

function GetCaptionFromEnum(aTypeInfo: pointer; aIndex: integer): string;
{$ifdef PUREPASCAL}
begin
  result := IntToStr(aIndex); // no GetEnumName() available
end;
{$else}
var PS: PUTF8Char;
    tmp: array[byte] of AnsiChar;
    L: integer;
begin
  PS := pointer(GetEnumName(aTypeInfo,aIndex));
  L := ord(PS^);
  inc(PS);
  while (L>0) and (PS^ in ['a'..'z']) do begin inc(PS); dec(L); end;
  tmp[L] := #0; // GetCaptionFromPCharLen expect
  move(PS^,tmp,L);
  GetCaptionFromPCharLen(tmp,result);
end;
{$endif}

{$ifdef LINUX}
const
  ANSI_CHARSET = 0;
  DEFAULT_CHARSET = 1;
  SYMBOL_CHARSET = 2;
  SHIFTJIS_CHARSET = $80;
  HANGEUL_CHARSET = 129;
  GB2312_CHARSET = 134;
  CHINESEBIG5_CHARSET = 136;
  OEM_CHARSET = 255;
  JOHAB_CHARSET = 130;
  HEBREW_CHARSET = 177;
  ARABIC_CHARSET = 178;
  GREEK_CHARSET = 161;
  TURKISH_CHARSET = 162;
  VIETNAMESE_CHARSET = 163;
  THAI_CHARSET = 222;
  EASTEUROPE_CHARSET = 238;
  RUSSIAN_CHARSET = 204;
  BALTIC_CHARSET = 186;
{$endif}

{$ifdef FPC}
const
  VIETNAMESE_CHARSET = 163;
{$endif}

function CharSetToCodePage(CharSet: integer): cardinal;
begin
  case CharSet of
    SHIFTJIS_CHARSET:   result := 932;
    HANGEUL_CHARSET:    result := 949;
    GB2312_CHARSET:     result := 936;
    HEBREW_CHARSET:     result := 1255;
    ARABIC_CHARSET:     result := 1256;
    GREEK_CHARSET:      result := 1253;
    TURKISH_CHARSET:    result := 1254;
    VIETNAMESE_CHARSET: result := 1258;
    THAI_CHARSET:       result := 874;
    EASTEUROPE_CHARSET: result := 1250;
    RUSSIAN_CHARSET:    result := 1251;
    BALTIC_CHARSET:     result := 1257;
  else result := CODEPAGE_US; // default is ANSI_CHARSET = iso-8859-1 = windows-1252
  end;
end;

function CodePageToCharSet(CodePage: Cardinal): Integer;
begin
  case CodePage of
    932:  result := SHIFTJIS_CHARSET;
    949:  result := HANGEUL_CHARSET;
    936:  result := GB2312_CHARSET;
    1255: result := HEBREW_CHARSET;
    1256: result := ARABIC_CHARSET;
    1253: result := GREEK_CHARSET;
    1254: result := TURKISH_CHARSET;
    1258: result := VIETNAMESE_CHARSET;
    874:  result := THAI_CHARSET;
    1250: result := EASTEUROPE_CHARSET;
    1251: result := RUSSIAN_CHARSET;
    1257: result := BALTIC_CHARSET;
  else result := ANSI_CHARSET; // default is iso-8859-1 = windows-1252
  end;
end;

function GetMimeContentType(Content: Pointer; Len: integer;
  const FileName: TFileName=''): RawUTF8;
begin // see http://www.garykessler.net/library/file_sigs.html for magic numbers
  result := '';
  if (Content<>nil) and (Len>4) then
    case PCardinal(Content)^ of
    $04034B50: Result := 'application/zip'; // 50 4B 03 04
    $46445025: Result := 'application/pdf'; //  25 50 44 46 2D 31 2E
    $21726152: Result := 'application/x-rar-compressed'; // 52 61 72 21 1A 07 00
    $AFBC7A37: Result := 'application/x-7z-compressed';  // 37 7A BC AF 27 1C
    $75B22630: Result := 'audio/x-ms-wma'; // 30 26 B2 75 8E 66
    $9AC6CDD7: Result := 'video/x-ms-wmv'; // D7 CD C6 9A 00 00
    $474E5089: Result := 'image/png'; // 89 50 4E 47 0D 0A 1A 0A
    $38464947: Result := 'image/gif'; // 47 49 46 38
    $002A4949, $2A004D4D, $2B004D4D:
      Result := 'image/tiff'; // 49 49 2A 00 or 4D 4D 00 2A or 4D 4D 00 2B
    $E011CFD0: // Microsoft Office applications D0 CF 11 E0 = DOCFILE
      if Len>600 then
      case PWordArray(Content)^[256] of // at offset 512
        $A5EC: Result := 'application/msword'; // EC A5 C1 00
        $FFFD: // FD FF FF
          case PByteArray(Content)^[516] of
            $0E,$1C,$43: Result := 'application/vnd.ms-powerpoint';
            $10,$1F,$20,$22,$23,$28,$29: Result := 'application/vnd.ms-excel';
          end;
      end;
    else
      case PCardinal(Content)^ and $00ffffff of
        $685A42: Result := 'application/bzip2'; // 42 5A 68
        $088B1F: Result := 'application/gzip'; // 1F 8B 08
        $492049: Result := 'image/tiff'; // 49 20 49
        $FFD8FF: Result := 'image/jpeg'; // FF D8 FF DB/E0/E1/E2/E3/E8
        else
          case PWord(Content)^ of
            $4D42: Result := 'image/bmp'; // 42 4D
          end;
      end;
    end;
  if (Result='') and (FileName<>'') then begin
    case GetFileNameExtIndex(FileName,'png,gif,tiff,tif,jpg,jpeg,bmp,doc,docx') of
      0:   Result := 'image/png';
      1:   Result := 'image/gif';
      2,3: Result := 'image/tiff';
      4,5: Result := 'image/jpeg';
      6:   Result := 'image/bmp';
      7,8: Result := 'application/msword';
      else begin
        Result := RawUTF8(ExtractFileExt(FileName));
        if Result<>'' then begin
          Result[1] := '/';
          Result := 'application'+LowerCase(Result);
        end;
      end;
    end;
  end;
  if Result='' then
    Result := 'application/octet-stream';
end;

function IsContentCompressed(Content: Pointer; Len: integer): boolean;
begin
  if (Content<>nil) and (Len>4) then
    case PCardinal(Content)^ of
    $04034B50, // 'application/zip'; // 50 4B 03 04
    $21726152, // 'application/x-rar-compressed'; // 52 61 72 21 1A 07 00
    $AFBC7A37, // 'application/x-7z-compressed'; // 37 7A BC AF 27 1C
    $75B22630, // 'audio/x-ms-wma'; // 30 26 B2 75 8E 66
    $9AC6CDD7, // 'video/x-ms-wmv'; // D7 CD C6 9A 00 00
    $474E5089, // 'image/png'; // 89 50 4E 47 0D 0A 1A 0A
    $38464947, // 'image/gif'; // 47 49 46 38
    $002A4949, $2A004D4D, $2B004D4D: // 'image/tiff';
      result := true;
    else
      case PCardinal(Content)^ and $00ffffff of
        $685A42, // 'application/bzip2'; // 42 5A 68
        $088B1F, // 'application/gzip'; // 1F 8B 08
        $492049, // 'image/tiff'; // 49 20 49
        $FFD8FF: // 'image/jpeg'; // FF D8 FF DB/E0/E1/E2/E3/E8
          result := true;
        else result := false;
      end;
    end else
    result := false;
end;

function FastLocatePUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char): PtrInt;
var L,i,cmp: PtrInt;
begin
  if R<0 then
    result := 0 else begin
    L := 0;
    result := -1; // return -1 if found
    repeat
      i := (L + R) shr 1;
      cmp := StrComp(P^[i],Value);
      if cmp=0 then
        exit;
      if cmp<0 then
        L := i + 1 else
        R := i - 1;
    until (L > R);
    while (i>=0) and (StrComp(P^[i],Value)>=0) do dec(i);
    result := i+1; // return the index where to insert
  end;
end;

function FastLocatePUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char;
  Compare: TUTF8Compare): PtrInt; overload;
var L,i,cmp: PtrInt;
begin
  if not Assigned(Compare) or (R<0) then
    result := 0 else begin
    L := 0;
    result := -1; // return -1 if found
    repeat
      i := (L + R) shr 1;
      cmp := Compare(P^[i],Value);
      if cmp=0 then
        exit;
      if cmp<0 then
        L := i + 1 else
        R := i - 1;
    until (L > R);
    while (i>=0) and (Compare(P^[i],Value)>=0) do dec(i);
    result := i+1; // return the index where to insert
  end;
end;

function FastFindPUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char;
  Compare: TUTF8Compare): PtrInt; overload;
var L, cmp: PtrInt;
begin
  L := 0;
  if Assigned(Compare) and (0<=R) then
  repeat
    result := (L + R) shr 1;
    cmp := Compare(P^[result],Value);
    if cmp=0 then
      exit;
    if cmp<0 then
      L := result + 1 else
      R := result - 1;
  until (L > R);
  result := -1;
end;

function FastFindPUTF8CharSorted(P: PPUTF8CharArray; R: PtrInt; Value: PUTF8Char): PtrInt;
// very fast find using a binary search
var L, cmp: PtrInt;
begin
  L := 0;
  if 0<=R then
  repeat
    result := (L + R) shr 1;
    cmp := StrComp(P^[result],Value);
    if cmp=0 then
      exit;
    if cmp<0 then
      L := result + 1 else
      R := result - 1;
  until (L > R);
  result := -1;
end;

function AddSortedRawUTF8(var Values: TRawUTF8DynArray; var ValuesCount: integer;
  const Value: RawUTF8; CoValues: PIntegerDynArray=nil; ForcedIndex: PtrInt=-1;
  Compare: TUTF8Compare=nil): PtrInt;
var n: PtrInt;
begin
  if ForcedIndex>=0 then
    result := ForcedIndex else begin
    if Assigned(Compare) then
      result := FastLocatePUTF8CharSorted(pointer(Values),ValuesCount-1,pointer(Value),Compare) else
      result := FastLocatePUTF8CharSorted(pointer(Values),ValuesCount-1,pointer(Value));
    if result<0 then
      exit; // Value exists -> fails
  end;
  n := Length(Values);
  if ValuesCount=n then begin
    inc(n,256+n shr 3);
    SetLength(Values,n);
    if CoValues<>nil then
      SetLength(CoValues^,n);
  end;
  n := ValuesCount;
  if result<n then begin
    n := (n-result)*sizeof(pointer);
    move(Pointer(Values[result]),Pointer(Values[result+1]),n);
    PtrInt(Values[result]) := 0; // avoid GPF
    if CoValues<>nil then begin
      {$ifdef CPU64}n := n shr 1;{$endif} // 64 bit pointer size is twice an integer
      move(CoValues^[result],CoValues^[result+1],n);
    end;
  end else
    result := n;
  Values[result] := Value;
  inc(ValuesCount);
end;


type
  /// used internaly for faster quick sort
  TQuickSortRawUTF8 = {$ifndef UNICODE}object{$else}record{$endif}
    Values: PPointerArray;
    Compare: TUTF8Compare;
    CoValues: PIntegerArray;
    Pivot: pointer;
    procedure Sort(L,R: PtrInt);
  end;

procedure TQuickSortRawUTF8.Sort(L, R: PtrInt);
var I, J, P: integer;
    Tmp: Pointer;
    TmpInt: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      pivot := Values^[P];
      while Compare(Values^[I],pivot)<0 do Inc(I);
      while Compare(Values^[J],pivot)>0 do Dec(J);
      if I <= J then begin
        Tmp := Values^[J];
        Values^[J] := Values^[I];
        Values^[I] := Tmp;
        if CoValues<>nil then begin
          TmpInt := CoValues^[J];
          CoValues^[J] := CoValues^[I];
          CoValues^[I] := TmpInt;
        end;
        if P = I then P := J else if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I > J;
    if L < J then
      Sort(L, J);
    L := I;
  until I >= R;
end;

procedure QuickSortRawUTF8(var Values: TRawUTF8DynArray; ValuesCount: integer;
  CoValues: PIntegerDynArray=nil; Compare: TUTF8Compare=nil);
var QS: TQuickSortRawUTF8;
begin
  QS.Values := pointer(Values);
  if Assigned(Compare) then
    QS.Compare := Compare else
    QS.Compare := StrComp;
  if CoValues=nil then
    QS.CoValues := nil else
    QS.CoValues := pointer(CoValues^);
  QS.Sort(0,ValuesCount-1);
end;

function DeleteRawUTF8(var Values: TRawUTF8DynArray; var ValuesCount: integer;
  Index: integer; CoValues: PIntegerDynArray=nil): boolean;
var n: integer;
begin
  n := ValuesCount;
  if Cardinal(Index)>=Cardinal(n) then
    result := false else begin
    dec(n);
    ValuesCount := n;
    Values[Index] := ''; // avoid GPF
    dec(n,Index);
    if n>0 then begin
      if CoValues<>nil then
        move(CoValues^[Index+1],CoValues^[Index],n*sizeof(Integer));
      move(pointer(Values[Index+1]),pointer(Values[Index]),n*sizeof(pointer));
      PtrUInt(Values[ValuesCount]) := 0; // avoid GPF
    end;
    result := true;
  end;
end;



{$ifdef DELPHI6OROLDER}
function GetFileVersion(const FileName: TFileName): cardinal;
var Size, Size2: DWord;
    Pt: Pointer;
    Info: ^TVSFixedFileInfo;
    tmp: TFileName;
begin
  result := cardinal(-1);
  if FileName='' then
    exit;
  // GetFileVersionInfo modifies the filename parameter data while parsing
  // Copy the string const into a local variable to create a writeable copy
  SetString(tmp,PChar(FileName),length(FileName));
  Size := GetFileVersionInfoSize(pointer(tmp), Size2);
  if Size>0 then begin
    GetMem(Pt, Size);
    try
      GetFileVersionInfo(pointer(FileName), 0, Size, Pt);
      if VerQueryValue(Pt, '\', pointer(Info), Size2) then
        result := Info^.dwFileVersionMS;
    finally
      Freemem(Pt);
    end;
  end;
end;
{$endif}

{$ifdef MSWINDOWS}

function WndProcMethod(Hwnd: HWND; Msg,wParam,lParam: integer): integer; stdcall;
var obj: TObject;
    dsp: TMessage;
begin
  {$ifdef CPU64}
  obj := pointer(GetWindowLongPtr(HWnd,GWLP_USERDATA));
  {$else}
  obj := pointer(GetWindowLong(HWnd,GWL_USERDATA)); // faster than GetProp()
  {$endif}
  if not Assigned(obj) then
    result := DefWindowProc(HWnd,Msg,wParam,lParam) else begin
    dsp.msg := Msg;
    dsp.wParam := WParam;
    dsp.lParam := lParam;
    dsp.result := 0;
    obj.Dispatch(dsp);
    result := dsp.result;
  end;
end;

function CreateInternalWindow(const aWindowName: string; aObject: TObject): HWND;
var TempClass: TWndClass;
begin
  result := 0;
  if GetClassInfo(HInstance, pointer(aWindowName), TempClass) then
    exit; // class name already registered -> fail
  fillchar(TempClass,sizeof(TempClass),0);
  TempClass.hInstance := HInstance;
  TempClass.lpfnWndProc := @DefWindowProc;
  TempClass.lpszClassName :=  pointer(aWindowName);
  WinApi.Windows.RegisterClass(TempClass);
  result := CreateWindowEx(WS_EX_TOOLWINDOW, pointer(aWindowName),
    '', WS_POPUP {!0}, 0, 0, 0, 0, 0, 0, HInstance, nil);
  if result=0 then
    exit; // impossible to create window -> fail
  {$ifdef CPU64}
  SetWindowLongPtr(result,GWLP_USERDATA,PtrInt(aObject));
  SetWindowLongPtr(result,GWLP_WNDPROC,PtrInt(@WndProcMethod));
  {$else}
  SetWindowLong(result,GWL_USERDATA,PtrInt(aObject)); // faster than SetProp()
  SetWindowLong(result,GWL_WNDPROC,PtrInt(@WndProcMethod));
  {$endif}
end;

function ReleaseInternalWindow(var aWindowName: string; var aWindow: HWND): boolean;
begin
  if (aWindow<>0) and (aWindowName<>'') then begin
    {$ifdef CPU64}
    SetWindowLongPtr(aWindow,GWLP_WNDPROC,PtrInt(@DefWindowProc));
    {$else}
    SetWindowLong(aWindow,GWL_WNDPROC,PtrInt(@DefWindowProc));
    {$endif}
    DestroyWindow(aWindow);
    WinApi.Windows.UnregisterClass(pointer(aWindowName),hInstance);
    aWindow := 0;
    aWindowName := '';
    result := true;
  end else
    result := false;
end;

procedure PatchCode(Old,New: pointer; Size: integer; Backup: pointer=nil);
var RestoreProtection, Ignore: DWORD;
    i: integer;
begin
  if VirtualProtect(Old, Size, PAGE_EXECUTE_READWRITE, RestoreProtection) then
  begin
    if Backup<>nil then
      for i := 0 to Size-1 do  // do not use Move() here
        PByteArray(Backup)^[i] := PByteArray(Old)^[i];
    for i := 0 to Size-1 do    // do not use Move() here
      PByteArray(Old)^[i] := PByteArray(New)^[i];
    VirtualProtect(Old, Size, RestoreProtection, Ignore);
    FlushInstructionCache(GetCurrentProcess, Old, Size);
  end;
end;

procedure PatchCodePtrUInt(Code: PPtrUInt; Value: PtrUInt);
begin
  PatchCode(Code,@Value,SizeOf(Code^));
end;

procedure RedirectCode(Func, RedirectFunc: Pointer; Backup: PPatchCode=nil);
var NewJump: packed record
    Code: byte;        // $e9 = jmp {relative}
    Distance: integer; // relative jump is 32 bit even on CPU64 
  end;
begin
  assert(sizeof(TPatchCode)=sizeof(NewJump));
  NewJump.Code := $e9;
  NewJump.Distance := PtrInt(RedirectFunc)-PtrInt(Func)-sizeof(NewJump);
  PatchCode(Func,@NewJump,sizeof(NewJump),Backup);
  {$ifndef LVCL}
  assert(pByte(Func)^=$e9);
  {$endif}
end;

procedure RedirectCodeRestore(Func: pointer; const Backup: TPatchCode);
begin
  PatchCode(Func,@Backup,sizeof(TPatchCode));
end;


{ TFileVersion }

constructor TFileVersion.Create(const FileName: TFileName;
  DefaultVersion: integer);
var Size, Size2: DWord;
    Pt: Pointer;
    Info: ^TVSFixedFileInfo;
    FileTime: TFILETIME;
    SystemTime: TSYSTEMTIME;
    tmp: TFileName;
begin
  Major := DefaultVersion;
  if FileName='' then
    exit;
  // GetFileVersionInfo modifies the filename parameter data while parsing.
  // Copy the string const into a local variable to create a writeable copy.
  SetString(tmp,PChar(FileName),length(FileName));
  Size := GetFileVersionInfoSize(pointer(tmp), Size2);
  if Size>0 then begin
    GetMem(Pt, Size);
    try
      GetFileVersionInfo(pointer(FileName), 0, Size, Pt);
      VerQueryValue(Pt, '\', pointer(Info), Size2);
      with Info^ do begin
        Major := dwFileVersionMS shr 16;
        Minor := word(dwFileVersionMS);
        Release := dwFileVersionLS shr 16;
        Build := word(dwFileVersionLS);
        BuildYear := 2010;
        if (dwFileDateLS<>0) and (dwFileDateMS<>0) then begin
          FileTime.dwLowDateTime:= dwFileDateLS; // built date from version info
          FileTime.dwHighDateTime:= dwFileDateMS;
          FileTimeToSystemTime(FileTime, SystemTime);
          fBuildDateTime := EncodeDate(
            SystemTime.wYear,SystemTime.wMonth,SystemTime.wDay);
          BuildYear := SystemTime.wYear;
        end;
      end;
    finally
      Freemem(Pt);
    end;
  end;
  Main := IntToString(Major)+'.'+IntToString(Minor);
  fDetailed := Main+ '.'+IntToString(Release)+'.'+IntToString(Build);
  if fBuildDateTime=0 then // get build date from file age
    fBuildDateTime := FileAgeToDateTime(FileName);
end;

function TFileVersion.Version32: integer;
begin
  result := Major shl 16+Minor shl 8+Release;
end;

{$else}

function GetTickCount: Cardinal;
begin
  result := (Int64(clock)*1000) div CLOCKS_PER_SEC; // ms result
end;

{$endif MSWINDOWS}

{$ifndef LVCL}

{ THeapMemoryStream = faster TMemoryStream using FastMM4/SynScaleMM heap,
  not windows.GlobalAlloc() }

const
  MemoryDelta = $8000; // 32 KB granularity (must be a power of 2)

function THeapMemoryStream.Realloc(var NewCapacity: Integer): Pointer;
// allocates memory from Delphi heap (FastMM4/SynScaleMM) and not windows.Global*()
// and uses bigger growing size -> a lot faster
var i: PtrInt;
begin
  if NewCapacity>0 then begin
    i := Seek(0,soFromCurrent); // no direct access to fSize -> use Seek() trick
    if NewCapacity=Seek(0,soFromEnd) then begin // avoid ReallocMem() if just truncate
      result := Memory;
      Seek(i,soFromBeginning);
      exit;
    end;
    NewCapacity := (NewCapacity + (MemoryDelta - 1)) and not (MemoryDelta - 1);
    Seek(i,soFromBeginning);
  end;
  Result := Memory;
  if NewCapacity <> Capacity then begin
    if NewCapacity = 0 then begin
      FreeMem(Memory);
      Result := nil;
    end else begin
      if Capacity = 0 then
        GetMem(Result, NewCapacity) else
        if NewCapacity > Capacity then // only realloc if necessary (grow up)
          ReallocMem(Result, NewCapacity) else
          NewCapacity := Capacity; // same capacity as before
      if Result = nil then
        raise EStreamError.Create('THeapMemoryStream'); // memory allocation bug
    end;
  end;
end;

{$endif LVCL}


{ TSortedWordArray }

function FastLocateWordSorted(P: PWordArray; R: integer; Value: word): PtrInt;
var L,cmp: PtrInt;
begin
  if R<0 then
    result := 0 else begin
    L := 0;
    repeat
      result := (L + R) shr 1;
      cmp := P^[result]-Value;
      if cmp=0 then begin
        result := -result-1; // return -(foundindex+1) if already exists
        exit;
      end;
      if cmp<0 then
        L := result + 1 else
        R := result - 1;
    until (L > R);
    while (result>=0) and (P^[result]>=Value) do dec(result);
    result := result+1; // return the index where to insert
  end;
end;

function TSortedWordArray.Add(aValue: Word): PtrInt;
begin
  result := FastLocateWordSorted(pointer(Values),Count-1,aValue);
  if result<0 then // aValue already exists in Values[] -> fails
    exit;
  if Count=length(Values) then
    SetLength(Values,Count+100);
  if result<Count then
    move(Values[result],Values[result+1],(Count-result)*2) else
    result := Count;
  Values[result] := aValue;
  inc(Count);
end;

function TSortedWordArray.IndexOf(aValue: Word): PtrInt;
var L,R: PtrInt;
    cmp: integer;
begin
  L := 0;
  R := Count-1;
  if 0<=R then
  repeat
    result := (L + R) shr 1;
    cmp := Values[result]-aValue;
    if cmp=0 then
      exit else
    if cmp<0 then
      L := result + 1 else
      R := result - 1;
  until (L > R);
  result := -1;
end;

{$ifdef PUREPASCAL}
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  if Value<0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1 else
  if Value>0 then
    // 1->1, 2->3..
    Value := (Value shl 1)-1;
    // 0->0
  result := ToVarUInt32(Value,Dest);
end;
{$else}
function ToVarInt32(Value: PtrInt; Dest: PByte): PByte;
asm
      test eax,eax
      jnl @pos
      neg eax
      add eax,eax
      jmp ToVarUInt32
@pos: jz @zer
      lea eax,[eax*2-1]
      jmp ToVarUInt32
@zer: mov [edx],al
      lea eax,[edx+1]
end;
{$endif}

function FromVarInt32(var Source: PByte): PtrInt;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  result := FromVarUInt32(Source);
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function ToVarUInt32Length(Value: PtrUInt): PtrUInt;
begin
  if Value<=$7f then
    result := 1 else
  if Value<$80 shl 7 then
    result := 2 else
  if Value<$80 shl 14 then
    result := 3 else
  if Value <$80 shl 21 then
    result := 4 else
    result := 5;
end;

function ToVarUInt32LengthWithData(Value: PtrUInt): PtrUInt;
begin
  if Value<=$7f then
    result := Value+1 else
  if Value<$80 shl 7 then
    result := Value+2 else
  if Value<$80 shl 14 then
    result := Value+3 else
  if Value<$80 shl 21 then
    result := Value+4 else
    result := Value+5;
end;

{$ifdef PUREPASCAL}
function ToVarUInt32(Value: PtrUInt; Dest: PByte): PByte;
begin
  if Value>$7f then
  repeat
    Dest^ := (Value and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  until Value<=$7f;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;
{$else}
function ToVarUInt32(Value: PtrUInt; Dest: PByte): PByte;
asm // eax=Value edx=Dest
    cmp eax,$7F
    ja @n
    mov [edx],al
    lea eax,[edx+1]
    ret
@n: mov ecx,eax
@s: and cl,$7F // handle two bytes per loop
    shr eax,7
    or cl,$80
    cmp eax,$7f
    mov [edx],cl
    lea edx,[edx+1]
    mov ecx,eax
    jbe @z
    and cl,$7F
    shr eax,7
    or cl,$80
    cmp eax,$7f
    mov [edx],cl
    mov ecx,eax
    lea edx,[edx+1]
    ja @s
@z: mov [edx],al
    lea eax,[edx+1]
end;
{$endif}

{$ifdef HASINLINE}
function FromVarUInt32(var Source: PByte): PtrUInt;
begin
  result := Source^;
  inc(Source);
  if result>$7f then
    result := (result and $7F) or FromVarUInt32Up128(Source);
end;
{$else}
function FromVarUInt32(var Source: PByte): PtrUInt;
var c: PtrUInt;
begin
  result := Source^;
  inc(Source);
  if result<=$7f then
    exit;
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c<=$7f shl 7 then
    exit; // Values between 128 and 16256
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c<=$7f shl 14 then
    exit; // Values between 16257 and 2080768
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c<=$7f shl 21 then
    exit; // Values between 2080769 and 266338304
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;
{$endif}

function FromVarUInt32High(var Source: PByte): PtrUInt;
var c: PtrUInt;
begin
  result := Source^;
  inc(Source);
  c := Source^ shl 7;
  inc(Source);
  result := result and $7F or c;
  if c<=$7f shl 7 then
    exit; // Values between 128 and 16256
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c<=$7f shl 14 then
    exit; // Values between 16257 and 2080768
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c<=$7f shl 21 then
    exit; // Values between 2080769 and 266338304
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function FromVarUInt32up128(var Source: PByte): PtrUInt;
var c: PtrUInt;
begin 
  result := Source^ shl 7;
  inc(Source);
  if result<=$7f shl 7 then
    exit; // Values between 128 and 16256
  c := Source^ shl 14;
  inc(Source);
  result := result and $3FFF or c;
  if c<=$7f shl 14 then
    exit; // Values between 16257 and 2080768
  c := Source^ shl 21;
  inc(Source);
  result := result and $1FFFFF or c;
  if c<=$7f shl 21 then
    exit; // Values between 2080769 and 266338304
  c := Source^ shl 28;
  inc(Source);
  result := result and $FFFFFFF or c;
end;

function ToVarUInt64(Value: QWord; Dest: PByte): PByte;
begin
  {$ifndef CPU64}
  if Value<MaxInt then begin
    result := ToVarUInt32(Int64Rec(Value).Lo,Dest);
    Exit;
  end;
  {$endif}
  if Value>$7f then
  repeat
    Dest^ := (byte(Value) and $7F) or $80;
    Value := Value shr 7;
    inc(Dest);
  until Value<=$7f;
  Dest^ := Value;
  inc(Dest);
  result := Dest;
end;

function FromVarUInt64(var Source: PByte): QWord;
var c,n: PtrUInt;
begin
  if Source^>$7f then begin
    n := 0;
    Result := Source^ and $7F;
    inc(Source);
    repeat
      c := Source^;
      inc(n,7);
      if c<=$7f then
        break;
      result := result or (QWord(c and $7f) shl n);
      inc(Source);
    until false;
    result := result or (QWord(c) shl n);
  end else
    result := Source^;
  inc(Source);
end;

function ToVarInt64(Value: Int64; Dest: PByte): PByte;
begin // 0=0,1=1,2=-1,3=2,4=-2...
{$ifdef CPU64}
  if Value<0 then
    // -1->2, -2->4..
    Value := (-Value) shl 1 else
  if Value>0 then
    // 1->1, 2->3..
    Value := (Value shl 1)-1;
    // 0->0
  result := ToVarUInt64(Value,Dest);
{$else}
  if Value<0 then
    // -1->2, -2->4..
    result := ToVarUInt64((-Value) shl 1,Dest) else
  if Value>0 then
    // 1->1, 2->3..
    result := ToVarUInt64((Value shl 1)-1,Dest) else begin
    // 0->0
    Dest^ := 0;
    inc(Dest);
    result := Dest;
  end;
{$endif}
end;

function FromVarInt64(var Source: PByte): Int64;
begin // 0=0,1=1,2=-1,3=2,4=-2...
  result := FromVarUInt64(Source);
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function GotoNextVarInt(Source: PByte): pointer;
begin
  if Source<>nil then begin
    while Source^>$7f do inc(Source);
    inc(Source);
  end;
  result := Source;
end;

function GotoNextVarString(Source: PByte): pointer;
var Len: PtrUInt;
begin
  Len := FromVarUInt32(Source);
  result := Pointer(PtrUInt(Source)+Len);
end;

function FromVarString(var Source: PByte): RawUTF8;
var Len: PtrUInt;
begin
  Len := FromVarUInt32(Source);
  SetString(Result,PAnsiChar(Source),Len);
  inc(Source,Len);
end;

{ ************ low-level RTTI types and conversion routines }

// some minimal RTTI const and types

type
  PDynArrayTypeInfo = ^TDynArrayTypeInfo;
  TDynArrayTypeInfo = packed record
    kind: Byte;
    NameLen: Byte;
    elSize: Longint;
    elType: ^PDynArrayTypeInfo;
    varType: Integer;
  end;

{$ifdef FPC}
const // from system.inc
  tkInteger       = 1;
  tkChar          = 2;
  tkEnumeration   = 3;
  tkFloat         = 4;
  tkSet           = 5;
  tkMethod        = 6;
  tkSString       = 7;
  tkString        = tkSString;
  tkLString       = 8;
  tkAString       = 9;
  tkWString       = 10;
  tkVariant       = 11;
  tkArray         = 12;
  tkRecord        = 13;
  tkInterface     = 14;
  tkClass         = 15;
  tkObject        = 16;
  tkWChar         = 17;
  tkBool          = 18;
  tkInt64         = 19;
  tkQWord         = 20;
  tkDynArray      = 21;
  tkInterfaceCorba = 22;
  tkProcVar       = 23;
  tkUString       = 24;
  tkHelper        = 26;

 // all potentially managed types
 tkManagedTypes   = [tkAstring,tkWstring,tkUstring,tkArray,
                    tkObject,tkRecord,tkDynArray,tkInterface,tkVariant];

 function aligntoptr(p : pointer): pointer; inline;
 begin
 {$ifdef FPC_REQUIRES_PROPER_ALIGNMENT}
   result := align(p,sizeof(p));
 {$else FPC_REQUIRES_PROPER_ALIGNMENT}
   result := p;
 {$endif FPC_REQUIRES_PROPER_ALIGNMENT}
 end;

{$else}
const
  tkEnumeration = 3;
  tkClass       = 7;
  tkLString     = 10;
  tkWString     = 11;
  tkRecord      = 14;
  tkDynArray    = 17;
{$ifdef UNICODE}
  tkUString     = 18;
{$endif}
{$endif}

type
  TFieldInfo = packed record
    TypeInfo: ^PDynArrayTypeInfo;
    Offset: Cardinal;
  end;
  TFieldTable = packed record
    Kind: byte;
    NameLen: byte; // = Name[0] = length(Name)
    Size: cardinal;
    Count: integer;
    Fields: array[0..0] of TFieldInfo;
  end;
  PFieldTable = ^TFieldTable;

function RecordEquals(const RecA, RecB; TypeInfo: pointer): boolean;
var FieldTable: PFieldTable absolute TypeInfo;
    F: integer;
    Field: ^TFieldInfo;
    Diff: cardinal;
    A, B: PAnsiChar;
begin
  A := @RecA;
  B := @RecB;
  if A=B then begin // both nil or same pointer
    result := true;
    exit;
  end;
  result := false;
  if FieldTable^.Kind<>tkRecord then
    exit; // raise Exception.CreateFmt('%s is not a record',[Typ^.Name]);
  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
  {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
  Field := @FieldTable^.Fields[0];
  Diff := 0;
  for F := 1 to FieldTable^.Count do begin
    Diff := Field^.Offset-Diff;
    if Diff<>0 then begin
      if not CompareMem(A,B,Diff) then
        exit; // binary block not equal
      inc(A,Diff);
      inc(B,Diff);
    end;
    case Field^.TypeInfo^^.Kind of
      tkLString:
        if PAnsiString(A)^<>PAnsiString(B)^ then
          exit;
      tkWString:
        if PWideString(A)^<>PWideString(B)^ then
          exit;
      {$ifdef UNICODE}
      tkUString:
        if PUnicodeString(A)^<>PUnicodeString(B)^ then
          exit;
      {$endif}
      else exit; // kind of field not handled
    end;
    Diff := sizeof(PtrUInt); // size of tkLString+tkWString+tkUString in record
    inc(A,Diff);
    inc(B,Diff);
    inc(Diff,Field^.Offset);
    inc(Field);
  end;
  if CompareMem(A,B,FieldTable.Size-Diff) then
    result := true;
end;

function RecordSaveLength(const Rec; TypeInfo: pointer): integer;
var FieldTable: PFieldTable absolute TypeInfo;
    F: integer;
    Field: ^TFieldInfo;
    P: PPtrUInt;
    R: PAnsiChar;
    DynArray: TDynArray;
    IntFieldTable: PFieldTable;
begin
  R := @Rec;
  if (FieldTable^.Kind<>tkRecord) or (R=nil) then begin
    result := 0; // should have been checked before
    exit; // raise Exception.CreateFmt('%s is not a record',[FieldTable^.NameLen]);
  end;
  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
  {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
  Field := @FieldTable.Fields[0];
  result := FieldTable.Size;
  for F := 1 to FieldTable.Count do begin
    P := pointer(R+Field.Offset);
    case Field.TypeInfo^^.Kind of
      tkDynArray: begin
        DynArray.Init(Field.TypeInfo^,P^);
        inc(result,DynArray.SaveToLength-sizeof(PtrUInt));
      end;
      tkLString:
        if P^=0 then
          dec(result,sizeof(PtrUInt)-1) else
          inc(result,ToVarUInt32LengthWithData(PInteger(P^-4)^)-sizeof(PtrUInt));
      tkWString {$ifdef UNICODE}, tkUString{$endif}:
        if P^=0 then
          dec(result,sizeof(PtrUInt)-1) else
          inc(result,ToVarUInt32LengthWithData(PInteger(P^-4)^*2)-sizeof(PtrUInt));
      tkRecord: begin
        inc(result,RecordSaveLength(P^,Field.TypeInfo^));
        IntFieldTable := pointer(Field.TypeInfo^);
        inc(PtrUInt(IntFieldTable),IntFieldTable^.NameLen);
        {$ifdef FPC}IntFieldTable := AlignToPtr(IntFieldTable);{$endif}
        dec(result,IntFieldTable^.Size);
      end;
      else begin
        result := 0;
        exit; // invalid/unhandled record content
      end;
    end;
    inc(Field);
  end;
end;

function RecordSave(const Rec; Dest: PAnsiChar; TypeInfo: pointer): PAnsiChar; overload;
var FieldTable: PFieldTable absolute TypeInfo;
    F, Len: integer;
    Diff: cardinal;
    Field: ^TFieldInfo;
    R: PAnsiChar;
    Kind: byte;
    DynArray: TDynArray;
    IntFieldTable: PFieldTable;
begin
  R := @Rec;
{  if FieldTable^.Kind<>tkRecord then begin // should have been checked before
    result := nil;
    exit; // raise Exception.CreateFmt('%s is not a record',[Typ^.Name]);
  end; }
  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
  {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
  Field := @FieldTable^.Fields[0];
  Diff := 0;
  for F := 1 to FieldTable^.Count do begin
    Diff := Field^.Offset-Diff;
    if Diff<>0 then begin
      move(R^,Dest^,Diff);
      inc(R,Diff);
      inc(Dest,Diff);
    end;
    Kind := Field.TypeInfo^^.Kind;
    case Kind of
    tkDynArray: begin
      DynArray.Init(Field.TypeInfo^,R^);
      Dest := DynArray.SaveTo(Dest);
      Diff := sizeof(PtrUInt); // size of tkDynArray in record
    end;
    tkLString, tkWString {$ifdef UNICODE}, tkUString{$endif}: begin
      if PPtrUInt(R)^=0 then
        Len := 0 else
        Len := PInteger(PPtrUInt(R)^-4)^; // fast length() of string type
      if Kind<>tkLString then
        Len := Len*2;
      Dest := pointer(ToVarUInt32(Len,pointer(Dest)));
      if Len>0 then begin
        move(pointer(PPtrUInt(R)^)^,Dest^,Len);
        inc(Dest,Len);
      end;
      Diff := sizeof(PtrUInt); // size of tkLString+tkWString+tkUString in record
    end;
    tkRecord: begin
      Dest := RecordSave(R^,Dest,Field.TypeInfo^);
      IntFieldTable := pointer(Field.TypeInfo^);
      inc(PtrUInt(IntFieldTable),IntFieldTable^.NameLen);
      {$ifdef FPC}IntFieldTable := AlignToPtr(IntFieldTable);{$endif}
      Diff := IntFieldTable^.Size; // size of the record in record
    end;
    else begin
      result := nil; // invalid/unhandled record content
      exit;
    end;
    end;
    inc(R,Diff);
    inc(Diff,Field.Offset);
    inc(Field);
  end;
  Diff := FieldTable^.Size-Diff;
  if Diff<>0 then begin
    move(R^,Dest^,Diff);
    result := Dest+Diff;
  end else
    result := Dest;
end;

function RecordSave(const Rec; TypeInfo: pointer): RawByteString; overload;
var Len: integer;
begin
  Len := RecordSaveLength(Rec,TypeInfo);
  SetString(result,nil,Len);
  if Len<>0 then
    RecordSave(Rec,pointer(result),TypeInfo);
end;

procedure _Finalize(p: Pointer; typeInfo: Pointer);
{$ifdef FPC}
  [external name 'FPC_FINALIZE'];
{$else}
asm
{$ifdef CPU64}
  {$ifdef CPUX64}
  .NOFRAME
  {$endif}
  mov r8,1 // rcx=p rdx=typeInfo r8=ElemCount
{$else}
  mov ecx,1
{$endif}
  jmp System.@FinalizeArray
end;
{$endif}

procedure RecordClear(var Dest; TypeInfo: pointer);
{$ifdef FPC}
  [external name 'FPC_FINALIZE'];
{$else}
asm
{$ifdef CPUX64}
  .NOFRAME
{$endif}
  jmp System.@FinalizeRecord
end;
{$endif}

function RecordLoad(var Rec; Source: PAnsiChar; TypeInfo: pointer): PAnsiChar;
var FieldTable: PFieldTable absolute TypeInfo;
    F, Len: integer;
    Diff: cardinal;
    Field: ^TFieldInfo;
    R: PAnsiChar;
    Kind: byte;
    DynArray: TDynArray;
    IntFieldTable: PFieldTable;
begin
  R := @Rec;
  if (FieldTable^.Kind<>tkRecord) or (R=nil) then begin
    result := nil; // should have been checked before
    exit; // raise Exception.CreateFmt('%s is not a record',[Typ^.Name]);
  end;
  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
  {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
  Field := @FieldTable^.Fields[0];
  if Source=nil then begin  // inline RecordClear() function
    for F := 1 to  FieldTable^.Count do begin
      _Finalize(R+Field^.Offset,Field^.TypeInfo^);
      inc(Field);
    end;
    result := nil;
    exit;
  end;
  Diff := 0;
  for F := 1 to FieldTable^.Count do begin
    Diff := Field^.Offset-Diff;
    if Diff<>0 then begin
      move(Source^,R^,Diff);
      inc(Source,Diff);
      inc(R,Diff);
    end;
    Kind := Field.TypeInfo^^.Kind;
    case Kind of
    tkDynArray: begin
      DynArray.Init(Field.TypeInfo^,R^);
      Source := DynArray.LoadFrom(Source);
      Diff := sizeof(PtrUInt); // size of tkDynArray in record
    end;
    tkLString, tkWString {$ifdef UNICODE}, tkUString{$endif}: begin
      Len := FromVarUInt32(PByte(Source));
      case Kind of
        tkLString: begin
          SetString(PRawByteString(R)^,Source,Len);
          {$ifdef UNICODE}
          { Delphi 2009+: set Code page for this AnsiString }
          if Len<>0 then
            PWord(PPtrUInt(R)^-12)^ := PWord(PtrUInt(Field.TypeInfo^)+
              Field.TypeInfo^^.NameLen+2)^;
          {$endif}
        end;
        tkWString:
          SetString(PWideString(R)^,PWideChar(Source),Len shr 1);
        {$ifdef UNICODE}
        tkUString:
          SetString(PString(R)^,PWideChar(Source),Len shr 1);
        {$endif}
      end;
      inc(Source,Len);
      Diff := sizeof(PtrUInt); // size of tkLString+tkWString+tkUString in record
    end;
    tkRecord: begin
      Source := RecordLoad(R^,Source,Field.TypeInfo^);
      IntFieldTable := pointer(Field.TypeInfo^);
      inc(PtrUInt(IntFieldTable),IntFieldTable^.NameLen);
      {$ifdef FPC}IntFieldTable := AlignToPtr(IntFieldTable);{$endif}
      Diff := IntFieldTable^.Size; // size of the record in record
    end;
    else begin
      result := nil;
      exit; // invalid record content
    end;
    end;
    inc(R,Diff);
    inc(Diff,Field.Offset);
    inc(Field);
  end;
  Diff := FieldTable^.Size-Diff;
  if Diff<>0 then begin
    move(Source^,R^,Diff);
    result := Source+Diff;
  end else
    result := Source;
end;

{$ifdef USEPACKAGES}

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm // same params than _CopyRecord{ dest, source, typeInfo: Pointer }
{$ifdef CPUX64}
  .NOFRAME
{$endif}
  jmp System.@CopyRecord
end;

{$else}

{$ifdef DELPHI5OROLDER}

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm // same params than _CopyRecord{ dest, source, typeInfo: Pointer }
  jmp System.@CopyRecord
end;

{$else}

{$ifdef ENHANCEDRTL}

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm // same params than _CopyRecord{ dest, source, typeInfo: Pointer }
  jmp System.@CopyRecord // our enhanced RTL already has the faster version
end;

{$else}

{$ifdef FPC}

function fpc_Copy_internal(Src, Dest, TypeInfo : Pointer) : SizeInt; [external name 'FPC_COPY'];

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
begin
  fpc_Copy_internal(@Source,@Dest,TypeInfo);
end;

{$else}

{$ifdef PUREPASCAL}

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm // same params than _CopyRecord{ dest, source, typeInfo: Pointer }
{$ifdef CPUX64}
  .NOFRAME
{$endif}
  jmp System.@CopyRecord
end;

{$else}

procedure RecordCopyInvoke;
asm
  call System.@CopyRecord
end;

procedure RecordCopy(var Dest; const Source; TypeInfo: pointer);
asm  // faster version of _CopyRecord{dest, source, typeInfo: Pointer} by AB
        { ->    EAX pointer to dest             }
        {       EDX pointer to source           }
        {       ECX pointer to typeInfo         }
        push ebp
        push ebx
        push esi
        push edi
        movzx ebx,byte ptr [ecx].TFieldTable.NameLen
        mov esi,edx                     // esi = source
        mov edi,eax                     // edi = dest
        add ebx,ecx                     // ebx = TFieldTable
        xor eax,eax                     // eax = current offset
        mov ebp,[ebx].TFieldTable.Count // ebp = TFieldInfo count
        mov ecx,[ebx].TFieldTable.Size
        test ebp,ebp
        jz @fullcopy
        push ecx                        // sizeof(record) on stack
        add ebx,offset TFieldTable.Fields[0]   // ebx = first TFieldInfo
@next:  mov ecx,[ebx].TFieldInfo.&Offset
        mov edx,[ebx].TFieldInfo.TypeInfo
        sub ecx,eax
        mov edx,[edx]
        jle @nomov
        lea esi,[esi+ecx]
        lea edi,[edi+ecx]
        neg ecx
@mov1:  mov al,[esi+ecx] // fast copy not destructable data
        mov [edi+ecx],al
        inc ecx
        jnz @mov1
@nomov: mov eax,edi
        movzx ecx,byte ptr [edx]    // data type
        cmp ecx,tkLString
        je @@LString
        jb @@err
{$ifdef UNICODE}
        cmp ecx,tkUString
        je @@UString
{$else} cmp ecx,tkDynArray
        je @@DynArray
{$endif}ja @@err
        jmp dword ptr [ecx*4+@@tab-tkWString*4]
@@Tab:  dd @@WString,@@Variant,@@Array,@@Record,@@Interface,@@err
        {$ifdef UNICODE}dd @@DynArray{$endif}
@@errv: mov al,reVarInvalidOp
        jmp @@err2
@@err:  mov al,reInvalidPtr
@@err2: pop edi
        pop esi
        pop ebx
        pop ebp
        jmp System.Error
        nop // all functions below have esi=source edi=dest
@@Array:
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        push dword ptr [edx+ecx].TFieldTable.Size
        push dword ptr [edx+ecx].TFieldTable.Count
        mov ecx,dword ptr [edx+ecx].TFieldTable.Fields[0]
        mov ecx,[ecx]
        mov edx,esi
        call System.@CopyArray
        pop eax // restore sizeof(Array)
        jmp @@finish
@@Record:
        movzx ecx,byte ptr [edx].TFieldTable.NameLen
        mov ecx,[edx+ecx].TFieldTable.Size
        push ecx
        mov ecx,edx
        mov edx,esi
        call RecordCopy
        pop eax // restore sizeof(Record)
        jmp @@finish
        nop; nop; nop
@@Variant:
{$ifdef DELPHI6OROLDER}
        mov edx,esi
        call System.@VarCopy
{$else} cmp dword ptr [VarCopyProc],0
        mov edx,esi
        jz @@errv
        call [VarCopyProc]
{$endif}mov eax,16
        jmp @@finish
{$ifdef DELPHI6OROLDER} nop; nop; {$endif}
@@Interface:
        mov edx,[esi]
        call System.@IntfCopy
        jmp @@fin4
        nop; nop; nop
@@DynArray:
        mov ecx,edx // ecx=TypeInfo
        mov edx,[esi]
        call System.@DynArrayAsg
        jmp @@fin4
@@WString:
{$ifndef LINUX}
        mov edx,[esi]
        call System.@WStrAsg
        jmp @@fin4
{$endif}
@@LString:
        mov edx,[esi]
        call System.@LStrAsg
{$ifdef UNICODE}
        jmp @@fin4
        nop; nop
@@UString:
        mov edx,[esi]
        call System.@UStrAsg
{$endif}
@@fin4: mov eax,4
@@finish:
        add esi,eax
        add edi,eax
        add eax,[ebx].TFieldInfo.&Offset
        dec ebp    // any other TFieldInfo?
        lea ebx,[ebx+8] // next TFieldInfo
        jnz @next
        pop ecx // ecx= sizeof(record)
@fullcopy:
        mov edx,edi
        sub ecx,eax
        mov eax,esi
        jle @nomov2
        call move
@nomov2:pop edi
        pop esi
        pop ebx
        pop ebp
end;
{$endif}
{$endif}
{$endif}
{$endif}
{$endif}


var
  JSONCustomParsers: array of record
    DynArrayTypeInfo: pointer;
    RecordTypeInfo: pointer;
    Reader: TDynArrayJSONCustomReader;
    Writer: TDynArrayJSONCustomWriter;
  end;

const
  NULCHAR: AnsiChar = #0;

function JSONCustomParsersDynArrayIndex(aTypeInfo: pointer): integer; {$ifdef HASINLINE}inline;{$endif}
begin
  if JSONCustomParsers<>nil then
    for result := 0 to length(JSONCustomParsers)-1 do
      with JSONCustomParsers[result] do
      if DynArrayTypeInfo=aTypeInfo then
        if not Assigned(Reader) and not Assigned(Writer) then
          break else
          exit;
  result := -1;
end;

function JSONCustomParsersRecordIndex(aTypeInfo: pointer): integer; {$ifdef HASINLINE}inline;{$endif}
begin
  if JSONCustomParsers<>nil then
    for result := 0 to length(JSONCustomParsers)-1 do
      with JSONCustomParsers[result] do
      if RecordTypeInfo=aTypeInfo then
        if not Assigned(Reader) and not Assigned(Writer) then
          break else
          exit;
  result := -1;
end;

function RecordLoadJSON(var Rec; JSON: PUTF8Char; TypeInfo: pointer;
  EndOfObject: PUTF8Char=nil): PUTF8Char;
var wasString, wasValid: boolean;
    custom: integer;
    EndOfObj: AnsiChar;
    Val: PUTF8Char;
begin // code below must match TTextWriter.AddRecordJSON
  result := nil;
  if JSON=nil then
    exit;
  if (@Rec=nil) or (TypeInfo=nil) or (PDynArrayTypeInfo(TypeInfo)^.kind<>tkRecord) then
    raise ESynException.Create('Invalid RecordLoadJSON call');
  if JSON^=' ' then repeat inc(JSON); if JSON^=#0 then exit; until JSON^<>' ';
  if PCardinal(JSON)^=JSON_BASE64_MAGIC_QUOTE then begin
    Val := GetJSONField(JSON,JSON,@wasString,@EndOfObj);
    if (Val=nil) or (not wasString) or
       (PInteger(Val)^ and $00ffffff<>JSON_BASE64_MAGIC) or
       (RecordLoad(Rec,pointer(Base64ToBin(Val+3)),TypeInfo)=nil) then
      exit; // invalid content
  end else begin
    custom := JSONCustomParsersRecordIndex(TypeInfo);
    if custom<0 then
      exit;
    JSON := JSONCustomParsers[custom].Reader(JSON,Rec,wasValid);
    if not wasValid then
      exit;
    EndOfObj := JSON^;
    inc(JSON);
  end;
  if JSON=nil then
    result := @NULCHAR else
    result := JSON;
  if EndOfObject<>nil then
    EndOfObject^ := EndOfObj;
end;


{ TDynArray }

function SortDynArrayByte(const A,B): integer;
begin
  result := byte(A)-byte(B);
end;

function SortDynArrayWord(const A,B): integer;
begin
  result := word(A)-word(B);
end;

function SortDynArrayInteger(const A,B): integer;
begin
  result := Integer(A)-Integer(B);
end;

function SortDynArrayCardinal(const A,B): integer;
begin
  result := Int64(cardinal(A))-Int64(cardinal(B));
end;

function SortDynArrayInt64(const A,B): integer;
begin
  result := Int64(A)-Int64(B);
end;

function SortDynArrayPointer(const A,B): integer;
begin
  result := PtrInt(A)-PtrInt(B);
end;

function SortDynArrayDouble(const A,B): integer;
begin
  if Double(A)<Double(B) then
    result := -1 else
  if Double(A)>Double(B) then
    result := 1 else
    result := 0;
end;

function SortDynArrayAnsiString(const A,B): integer;
begin
  result := StrComp(PUTF8Char(A),PUTF8Char(B));
end;

function SortDynArrayString(const A,B): integer;
begin
  {$ifdef UNICODE}
  result := System.SysUtils.StrComp(PChar(A),PChar(B));
  {$else}
  result := StrComp(PUTF8Char(A),PUTF8Char(B));
  {$endif}
end;

function SortDynArrayAnsiStringI(const A,B): integer;
begin
  result := StrIComp(PUTF8Char(A),PUTF8Char(B));
end;

function SortDynArrayStringI(const A,B): integer;
begin
  {$ifdef UNICODE}
  result := AnsiICompW(PWideChar(A),PWideChar(B));
  {$else}
  result := StrIComp(PUTF8Char(A),PUTF8Char(B));
  {$endif}
end;

function SortDynArrayUnicodeString(const A,B): integer;
begin
  result := StrCompW(PWideChar(A),PWideChar(B));
end;

function SortDynArrayUnicodeStringI(const A,B): integer;
begin
  result := AnsiICompW(PWideChar(A),PWideChar(B));
end;

{$ifdef FPC}

function RTTIArraySize(typeInfo: Pointer): SizeInt;
type
  PArrayInfo=^TArrayInfo;
  TArrayInfo=packed record
    ElSize: SizeInt;
    ElCount: SizeInt;
    ElInfo: Pointer;
  end;
begin
  typeInfo := aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  result := PArrayInfo(typeInfo)^.ElSize * PArrayInfo(typeInfo)^.ElCount;
end;

function RTTIRecordSize(typeInfo: Pointer): SizeInt;
type
  PRecordInfo=^TRecordInfo;
  TRecordInfo=packed record
    Size: Longint;
    Count: Longint;
    { Elements: array[count] of TRecordElement }
  end;
begin
  typeInfo := aligntoptr(typeInfo+2+PByte(typeInfo)[1]);
  result := PRecordInfo(typeInfo)^.Size;
end;

function RTTISize(typeInfo: Pointer): SizeInt;
begin
  case PByte(typeinfo)^ of
    tkAString,tkWString,tkUString,
    tkInterface,tkDynarray:
      result:=sizeof(Pointer);
{$ifdef FPC_HAS_FEATURE_VARIANTS}
    tkVariant:
      result:=sizeof(TVarData);
{$endif FPC_HAS_FEATURE_VARIANTS}
    tkArray:
      result:=RTTIArraySize(typeinfo);
    tkObject,tkRecord:
      result:=RTTIRecordSize(typeinfo);
  else
    result:=-1;
  end;
end;

procedure CopyArray(dest, source, typeInfo: Pointer; cnt: PtrUInt);
var i, size: SizeInt;
begin
  size := RTTISize(typeInfo);
  if size>0 then
    for i := 0 to cnt-1 do
      fpc_copy_internal(source+size*i,dest+size*i,typeInfo);
end;

{$else}
procedure CopyArray(dest, source, typeInfo: Pointer; cnt: PtrUInt);
asm
{$ifdef CPU64}     
  {$ifdef CPUX64}
  .NOFRAME
  {$endif}
  jmp System.@CopyArray
{$else}
  push dword ptr [EBP+8]
  call System.@CopyArray
{$endif}
end;
{$endif}

function TDynArray.Add(const Elem): integer;
var P: pointer;
begin
  result := Count;
  if Value=nil then
    exit; // avoid GPF if void
  SetCount(result+1);
  P := pointer(PtrUInt(Value^)+PtrUInt(result)*ElemSize);
  if ElemType=nil then
    move(Elem,P^,ElemSize) else
    CopyArray(P,@Elem,ElemType,1);
end;

function TDynArray.New: integer;
begin
  result := Count;
  if Value=nil then
    exit; // avoid GPF if void
  SetCount(result+1);
end;

procedure TDynArray.Insert(Index: Integer; const Elem);
var n: integer;
    P: PByteArray;
begin
  if Value=nil then
    exit; // avoid GPF if void
  n := Count;
  SetCount(n+1);
  if cardinal(Index)<cardinal(n) then begin
    P := pointer(PtrUInt(Value^)+PtrUInt(Index)*ElemSize);
    Move(P[0],P[ElemSize],cardinal(n-Index)*ElemSize);
    if ElemType<>nil then
      fillchar(P[0],ElemSize,0); // avoid GPF
  end else
    // Index>=Count -> add at the end
    P := pointer(PtrUInt(Value^)+PtrUInt(n)*ElemSize);
  if ElemType=nil then
    move(Elem,P^,ElemSize) else
    CopyArray(P,@Elem,ElemType,1);
end;

procedure TDynArray.Clear;
begin
  SetCount(0);
end;

procedure TDynArray.Delete(Index: Integer);
var n, len: integer;
    P: PAnsiChar;
begin
  if Value=nil then
    exit; // avoid GPF if void
  n := Count;
  if cardinal(Index)>=cardinal(n) then
    exit; // out of range
  dec(n);
  P := pointer(PtrUInt(Value^)+PtrUInt(Index)*ElemSize);
  if ElemType<>nil then
    _Finalize(P,ElemType);
  if n>Index then begin
    len := cardinal(n-Index)*ElemSize;
    move(P[ElemSize],P[0],len);
    if ElemType<>nil then // avoid GPF
      fillchar(P[len],ElemSize,0);
  end;
  SetCount(n);
end;

function TDynArray.GetCount: integer;
begin
  if Value<>nil then
    if fCountP=nil then
      if PtrInt(Value^)<>0 then
        result := PInteger(PtrUInt(Value^)-sizeof(Integer))^ else
        result := 0 else
      result := fCountP^ else
    result := 0; // avoid GPF if void
end;

procedure Exchg(P1,P2: PAnsiChar; max: integer);
var c: AnsiChar;
    i: integer;
begin
  for i := 0 to max do begin
    c := P1[i];
    P1[i] := P2[i];
    P2[i] := c;
  end;
end;

procedure TDynArray.Reverse;
var i, siz, n, tmp: integer;
    P1, P2: PAnsiChar;
    c: AnsiChar;
    i64: Int64;
begin
  n := Count-1;
  if n>0 then begin
    siz := ElemSize;
    P1 := Value^;
    case siz of
    1: begin
      // optimized version for TByteDynArray and such
      P2 := P1+n;
      for i := 1 to n shr 1 do begin
        c := P1^;
        P1^ := P2^;
        P2^ := c;
        inc(P1);
        dec(P2);
      end;
    end;
    4: begin
      // optimized version for TIntegerDynArray + TRawUTF8DynArray and such
      P2 := P1+n*4;
      for i := 1 to n shr 1 do begin
        tmp := PInteger(P1)^;
        PInteger(P1)^ := PInteger(P2)^;
        PInteger(P2)^ := tmp;
        inc(P1,4);
        dec(P2,4);
      end;
    end;
    8: begin
      // optimized version for TInt64DynArray + TDoubleDynArray and such
      P2 := P1+n*8;
      for i := 1 to n shr 1 do begin
        i64:= PInt64(P1)^;
        PInt64(P1)^ := PInt64(P2)^;
        PInt64(P2)^ := i64;
        inc(P1,8);
        dec(P2,8);
      end;
    end;
    else begin
      // generic version
      P2 := P1+n*siz;
      for i := 1 to n shr 1 do begin
        Exchg(P1,P2,siz-1);
        inc(P1,siz);
        dec(P2,siz);
      end;
    end;
    end;
  end;
end;

procedure TDynArray.SaveToStream(Stream: TStream);
var Posi, PosiEnd: Integer;
    MemStream: TCustomMemoryStream absolute Stream;
    tmp: RawByteString;
begin
  if (Value=nil) or (Stream=nil) then
    exit; // avoid GPF if void
  if Stream.InheritsFrom(TCustomMemoryStream) then begin
    Posi := MemStream.Seek(0,soFromCurrent);
    PosiEnd := Posi+SaveToLength;
    if PosiEnd>MemStream.Size then
      MemStream.Size := PosiEnd;
    if SaveTo(PAnsiChar(MemStream.Memory)+Posi)-MemStream.Memory<>PosiEnd then
      EStreamError.Create('TDynArray.SaveToStream');
    MemStream.Seek(PosiEnd,soFromBeginning);
  end else begin
    tmp := SaveTo;
    Stream.Write(pointer(tmp)^,length(tmp));
  end;
end;

procedure TDynArray.LoadFromStream(Stream: TCustomMemoryStream);
var P: PAnsiChar;
begin
  P := PAnsiChar(Stream.Memory)+Stream.Seek(0,soFromCurrent);
  Stream.Seek(LoadFrom(P)-P,soFromCurrent);
end;

function TDynArray.SaveTo(Dest: PAnsiChar): PAnsiChar;
var i, n, Len: integer;
    P: PAnsiChar;
    FieldTable: PFieldTable;
begin
  if Value=nil then begin
    result := Dest;
    exit; // avoid GPF if void
  end;
  // first store the element size+type to check for the format (name='' mostly)
  Dest := pointer(ToVarUInt32(ElemSize,pointer(Dest)));
  if ElemType=nil then
    Dest^ := #0 else
    Dest^ := PAnsiChar(ElemType)^;
  inc(Dest);
  // then store dynamic array count
  n := Count;
  Dest := pointer(ToVarUInt32(n,pointer(Dest)));
  if n=0 then begin
    result := Dest;
    exit;
  end;
  inc(Dest,sizeof(Cardinal)); // leave space for Hash32 checksum
  result := Dest;
  // store dynamic array elements content
  P := Value^;
  if ElemType=nil then begin
    // binary types: store as once
    n := n*integer(ElemSize);
    move(P^,Dest^,n);
    inc(Dest,n);
  end else
    case PByte(ElemType)^ of
    tkLString, tkWString {$ifdef UNICODE}, tkUString{$endif}: begin
      for i := 1 to n do begin
        if PPtrUInt(P)^=0 then
          Len := 0 else begin
          Len := PInteger(PPtrUInt(P)^-4)^; // fast length() of string type
          if PByte(ElemType)^<>tkLString then
            Len := Len*2;
        end;
        Dest := pointer(ToVarUInt32(Len,pointer(Dest)));
        if Len>0 then begin
          move(pointer(PPtrUInt(P)^)^,Dest^,Len);
          inc(Dest,Len);
        end;
        inc(P,sizeof(PtrUInt));
      end;
    end;
    tkRecord: begin
      FieldTable := ElemType;
      inc(PtrUInt(FieldTable),FieldTable^.NameLen);
      {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
      if FieldTable^.Count=0 then begin
        {$ifndef LVCL}
        assert(FieldTable^.Size=ElemSize);
        {$endif}
        n := n*integer(ElemSize);
        move(P^,Dest^,n);
        inc(Dest,n);
      end else begin
        for i := 1 to n do begin
          Dest := RecordSave(P^,Dest,ElemType);
          if Dest=nil then
            break; // invalid record type (wrong field type)
          inc(P,ElemSize);
        end;
      end;
    end;
  end;
  // store Hash32 checksum
  if Dest<>nil then  // may be nil if RecordSave() failed
    PCardinal(result-sizeof(Cardinal))^ := Hash32(result,Dest-result);
  result := Dest;
end;

function TDynArray.SaveToLength: integer;
var i,n,L: integer;
    P: PAnsiChar;
begin
  if Value=nil then begin
    result := 0;
    exit; // avoid GPF if void
  end;
  n := Count;
  result := ToVarUInt32Length(ElemSize)+ToVarUInt32Length(n)+1;
  if n=0 then
    exit;
  if ElemType=nil then
    inc(result,integer(ElemSize)*n) else begin
    P := Value^;
    case PByte(ElemType)^ of
    tkLString:
      for i := 1 to n do begin
        if PPtrUInt(P)^=0 then
          inc(result) else
          inc(result,ToVarUInt32LengthWithData(PInteger(PPtrUInt(P)^-4)^));
        inc(P,sizeof(PtrUInt));
      end;
    tkWString {$ifdef UNICODE}, tkUString{$endif}:
      for i := 1 to n do begin
        if PPtrUInt(P)^=0 then
          inc(result) else
          inc(result,ToVarUInt32LengthWithData(PInteger(PPtrUInt(P)^-4)^*2));
        inc(P,sizeof(PtrUInt));
      end;
    tkRecord: begin
      for i := 1 to n do begin
        L := RecordSaveLength(P^,ElemType);
        if L=0 then
          break; // invalid record type (wrong field type)
        inc(result,L);
        inc(P,ElemSize);
      end;
    end;
    end;
  end;
  inc(result,sizeof(Cardinal)); // Hash32 checksum
end;

function TDynArray.SaveTo: RawByteString;
var Len: integer;
begin
  Len := SaveToLength;
  SetString(result,nil,Len);
  if Len<>0 then
    if SaveTo(pointer(result))-pointer(result)<>Len then
      raise ESynException.Create('TDynArray.SaveTo');
end;

function JSONArrayCount(P: PUTF8Char): integer;
var n: integer;
begin
  result := -1;
  n := 0;
  P := GotoNextNotSpace(P);
  if P^<>']' then
  repeat
    case P^ of
    '"': begin // "string \"field"
      repeat
        inc(P);
        if P^=#0 then exit else // unexpected end
        if P^='"' then
          if P[-1]<>'\' then  // 'sdklds\"' is invalid ending
            break else
            if P[-2]='\' then break; //   '\\"' is valid ending
      until false;
      inc(P);
    end;
    '{','[': begin
      P := GotoNextJSONObjectOrArray(P);
      if P=nil then
        exit; // invalid content
    end;
    end;
    while not (P^ in [#0,',',']']) do inc(P);
    inc(n);
    if P^<>',' then break;
    repeat inc(P) until not(P^ in [#1..' ']);
  until false;
  if P^=']' then
    result := n;
end;

function TypeInfoToKnown(aDynArrayTypeInfo: pointer): TDynArrayKind;
begin
 if aDynArrayTypeInfo=TypeInfo(TByteDynArray) then
   result := djByte else
  if aDynArrayTypeInfo=TypeInfo(TWordDynArray) then
   result := djWord else
  if aDynArrayTypeInfo=TypeInfo(TIntegerDynArray) then
   result := djInteger else
  if aDynArrayTypeInfo=TypeInfo(TCardinalDynArray) then
   result := djCardinal else
  if aDynArrayTypeInfo=TypeInfo(TInt64DynArray) then
   result := djInt64 else
  if aDynArrayTypeInfo=TypeInfo(TDoubleDynArray) then
   result := djDouble else
  if aDynArrayTypeInfo=TypeInfo(TCurrencyDynArray) then
   result := djCurrency else
  if (aDynArrayTypeInfo=TypeInfo(TRawUTF8DynArray)) or
     (aDynArrayTypeInfo=TypeInfo(TRawByteStringDynArray)) then
   result := djRawUTF8 else
  if aDynArrayTypeInfo=TypeInfo(TWinAnsiDynArray) then
    result := djWinAnsi else
  if aDynArrayTypeInfo=TypeInfo(TStringDynArray) then
    result := djString else
  if aDynArrayTypeInfo=TypeInfo(TTimeLogDynArray) then
    result := djTimeLog else
  if aDynArrayTypeInfo=TypeInfo(TDateTimeDynArray) then
    result := djDateTime else
  if aDynArrayTypeInfo=TypeInfo(TWideStringDynArray) then
    result := djWideString else
  if aDynArrayTypeInfo=TypeInfo(TSynUnicodeDynArray) then
    result := djSynUnicode else
    result := djNone;
end;

function TypeInfoToRecordInfo(aDynArrayTypeInfo: pointer): pointer;
var Typ: PDynArrayTypeInfo absolute aDynArrayTypeInfo;
begin
  result := nil;
  if (aDynArrayTypeInfo<>nil) and (Typ^.kind=tkDynArray) then begin
    inc(PtrUInt(Typ),Typ^.NameLen);
    {$ifdef FPC}Typ := AlignToPtr(Typ);{$endif}
    if Typ^.elType<>nil then
      result := Typ^.elType{$ifndef FPC}^{$endif};
  end;
end;

function TDynArray.ToKnownType: TDynArrayKind;
var FieldTable: PFieldTable;
label Bin, Rec;
begin
  if fKnownType<>djNone then begin
    result := fKnownType;
    exit;
  end;
  fKnownType := TypeInfoToKnown(TypeInfo);
  if fKnownType=djNone then
    if ElemType=nil then
  Bin:case ElemSize of
      1: fKnownType := djByte;
      2: fKnownType := djWord;
      4: fKnownType := djInteger;
      8: fKnownType := djInt64;
      end else
    case PByte(ElemType)^ of
      tkLString: fKnownType := djRawUTF8;
      tkWString{$ifdef UNICODE}, tkUString{$endif}: fKnownType := djSynUnicode;
      tkRecord: begin
        FieldTable := ElemType;
  Rec:  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
  {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
        if FieldTable^.Count=0 then // only binary content -> full content
          goto Bin else
          with FieldTable^.Fields[0] do
          case Offset of
          0: case TypeInfo^^.Kind of
              tkLString: fKnownType := djRawUTF8;
              tkWString{$ifdef UNICODE}, tkUString{$endif}: fKnownType := djSynUnicode;
              tkRecord: begin
                FieldTable := pointer(TypeInfo^);
                goto Rec;
              end;
             end;
          1: fKnownType := djByte;
          2: fKnownType := djWord;
          4: fKnownType := djInteger;
          8: fKnownType := djInt64;
          end;
      end;
    end;
  result := fKnownType;
end;

const
  SORTFIRSTFIELD: array[boolean,TDynArrayKind] of TDynArraySortCompare = (
    (nil, SortDynArrayByte, SortDynArrayWord, SortDynArrayInteger,
    SortDynArrayCardinal, SortDynArrayInt64, SortDynArrayDouble,
    SortDynArrayInt64, SortDynArrayInt64, SortDynArrayDouble,
    SortDynArrayAnsiString, SortDynArrayAnsiString, SortDynArrayString,
    SortDynArrayUnicodeString, SortDynArrayUnicodeString, nil),
    (nil, SortDynArrayByte, SortDynArrayWord, SortDynArrayInteger,
    SortDynArrayCardinal, SortDynArrayInt64, SortDynArrayDouble,
    SortDynArrayInt64, SortDynArrayInt64, SortDynArrayDouble,
    SortDynArrayAnsiStringI, SortDynArrayAnsiStringI, SortDynArrayStringI,
    SortDynArrayUnicodeStringI, SortDynArrayUnicodeStringI, nil));

function TDynArray.LoadFromJSON(P: PUTF8Char): PUTF8Char;
var n, i, custom: integer;
    T: TDynArrayKind;
    wasString, expectedString, isValid: boolean;
    EndOfObject: AnsiChar;
    Val: PUTF8Char;
begin // code below must match TTextWriter.AddDynArrayJSON()
  result := nil;
  if (P=nil) or (Value=nil) then
    exit;
  P := GotoNextNotSpace(P);
  if P^<>'[' then
    exit;
  repeat inc(P) until P^<>' ';
  n := JSONArrayCount(P);
  if n<0 then
    exit; // invalid array content
  if n=0 then begin
    P := GotoNextNotSpace(P);
    if P^<>']' then
      result := nil else begin
      Clear;
      result := P+1;
    end;
    exit; // handle '[]' array
  end;
  custom := JSONCustomParsersDynArrayIndex(TypeInfo);
  if custom>=0 then
    T := djCustom else
    T := TypeInfoToKnown(TypeInfo);
  if (T=djNone) or
     (PCardinal(P)^=JSON_BASE64_MAGIC_QUOTE) then begin
    if n<>1 then
      exit; // expect one Base64 encoded string value preceded by \uFFF0
    Val := GetJSONField(P,P,@wasString,@EndOfObject);
    if (Val=nil) or (not wasString) or
       (PInteger(Val)^ and $00ffffff<>JSON_BASE64_MAGIC) or
       (LoadFrom(pointer(Base64ToBin(Val+3)))=nil) then
      exit; // invalid content
  end else begin
    Count := n; // fast allocation of the whole dynamic array memory at once
    if T=djCustom then begin
      Val := Value^;
      with JSONCustomParsers[custom] do
      if not Assigned(Reader) then
        exit else
      for i := 1 to n do begin
        P := Reader(P,Val^,isValid);
        if not isValid then
          exit;
        EndOfObject := P^; // ',' or ']' for the last item of the array
        inc(P);
        inc(Val,ElemSize);
      end;
    end else begin
      expectedString := (T in [djTimeLog..djSynUnicode]);
      for i := 0 to n-1 do begin
        Val := GetJSONField(P,P,@wasString,@EndOfObject);
        if (Val=nil) or (wasString<>expectedString) then
          exit;
        case T of
        djByte:     PByteArray(Value^)^[i] := GetCardinal(Val);
        djWord:     PWordArray(Value^)^[i] := GetCardinal(Val);
        djInteger:  PIntegerArray(Value^)^[i] := GetInteger(Val);
        djCardinal: PCardinalArray(Value^)^[i] := GetCardinal(Val);
        djInt64:    SetInt64(Val,PInt64Array(Value^)^[i]);
        djTimeLog:  PInt64Array(Value^)^[i] := Iso8601ToSecondsPUTF8Char(Val,0);
        djDateTime: Iso8601ToDateTimePUTF8CharVar(Val,0,TDateTime(PDoubleArray(Value^)^[i]));
        djDouble:   PDoubleArray(Value^)^[i] := GetExtended(Val);
        djCurrency: PInt64Array(Value^)^[i] := StrToCurr64(Val);
        djRawUTF8:  RawUTF8(PPointerArray(Value^)^[i]) := Val;
        djWinAnsi:  WinAnsiString(PPointerArray(Value^)^[i]) := WinAnsiConvert.UTF8BufferToAnsi(Val,StrLen(Val));
        djString:   string(PPointerArray(Value^)^[i]) := UTF8DecodeToString(Val,StrLen(Val));
        djWideString: UTF8ToWideString(Val,StrLen(Val),WideString(PPointerArray(Value^)^[i]));
        djSynUnicode: SynUnicode(PPointerArray(Value^)^[i]) := UTF8ToSynUnicode(Val);
        end;
      end;
    end;
  end;
  if EndOfObject=']' then
    if P=nil then
      result := @NULCHAR else
      result := P;
end;

function SimpleDynArrayLoadFrom(Source: PAnsiChar; aTypeInfo: pointer;
  var Count, ElemSize: integer): pointer;
var Hash: PCardinalArray absolute Source;
    Typ: PDynArrayTypeInfo absolute aTypeInfo;
begin
  result := nil;
  if (aTypeInfo=nil) or (Typ^.kind<>tkDynArray) then
    exit; // invalid type information
  inc(PtrUInt(Typ),Typ^.NameLen);
  {$ifdef FPC}Typ := AlignToPtr(Typ);{$endif}
  if (Typ^.ElType<>nil) or (Source=nil) or
     (Source[0]<>AnsiChar(Typ^.elSize)) or (Source[1]<>#0) then
    exit; // invalid type information or Source content
  ElemSize := Typ^.elSize;
  inc(Source,2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  if (Count<>0) and (Hash32(@Hash[1],Count*Typ^.elSize)=Hash[0]) then
    result := @Hash[1]; // returns valid Source content
end;

function IntegerDynArrayLoadFrom(Source: PAnsiChar; var Count: integer): PIntegerArray;
var Hash: PCardinalArray absolute Source;
begin
  result := nil;
  if (Source=nil) or (Source[0]<>#4) or (Source[1]<>#0) then
    exit; // invalid Source content
  inc(Source,2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  if (Count<>0) and (Hash32(@Hash[1],Count*4)=Hash[0]) then
    result := @Hash[1]; // returns valid Source content
end;

function RawUTF8DynArrayLoadFromContains(Source: PAnsiChar;
  Value: PUTF8Char; ValueLen: integer; CaseSensitive: boolean): integer;
var Count, Len: integer;
begin
  if (Value=nil) or (ValueLen=0) or
     (Source=nil) or (Source[0]<>#4) or (Source[1]<>AnsiChar(tkLString)) then begin
    result := -1;
    exit; // invalid Source or Value content
  end;
  inc(Source,2);
  Count := FromVarUInt32(PByte(Source)); // dynamic array count
  inc(Source,sizeof(cardinal)); // ignore security checksum
  for result := 0 to Count-1 do begin
    Len := FromVarUInt32(PByte(Source));
    if CaseSensitive then begin
      if (Len=ValueLen) and CompareMem(Value,Source,Len) then
        exit;
    end else
      if UTF8ILComp(Value,pointer(Source),ValueLen,Len)=0 then
        exit;
    inc(Source,Len);
  end;
  result := -1;
end;

function TDynArray.LoadFrom(Source: PAnsiChar): PAnsiChar;
var i, n, Len: integer;
    P: PAnsiChar;
    FieldTable: PFieldTable;
    Hash: PCardinalArray;
begin
  // check stored element size+type
  if Source=nil then begin
    Clear;
    result := nil;
    exit;
  end;
  if (Value=nil) or (FromVarUInt32(PByte(Source))<>ElemSize) or
     ((ElemType=nil) and (Source^<>#0) or
     ((ElemType<>nil) and (Source^<>PAnsiChar(ElemType)^))) then begin
    result := nil; // invalid Source content
    exit;
  end;
  inc(Source);
  // retrieve dynamic array count
  n := FromVarUInt32(PByte(Source));
  SetCount(n);
  if n=0 then begin
    result := Source;
    exit;
  end;
  // retrieve security checksum
  Hash := pointer(Source);
  inc(Source,sizeof(cardinal));
  // retrieve dynamic array elements content
  P := Value^;
  if ElemType=nil then begin
    // binary type was stored as once
    n := n*integer(ElemSize);
    move(Source^,P^,n);
    inc(Source,n);
  end else
    case PByte(ElemType)^ of
      tkLString, tkWString {$ifdef UNICODE}, tkUString{$endif}:
      for i := 1 to n do begin
        Len := FromVarUInt32(PByte(Source));
        case PByte(ElemType)^ of
        tkLString: begin
          SetString(PRawByteString(P)^,Source,Len);
          {$ifdef UNICODE}
          { Delphi 2009+: set Code page for this AnsiString }
          if Len<>0 then
            PWord(PPtrUInt(P)^-12)^ := PWord(PtrUInt(ElemType)+
              PDynArrayTypeInfo(ElemType)^.NameLen+2)^;
          {$endif}
        end;
        tkWString:
          SetString(PWideString(P)^,PWideChar(Source),Len shr 1);
        {$ifdef UNICODE}
        tkUString:
          SetString(PString(P)^,PWideChar(Source),Len shr 1);
        {$endif}
        end;
        inc(Source,Len);
        inc(P,sizeof(PtrUInt));
      end;
      tkRecord: begin
        FieldTable := ElemType;
        inc(PtrUInt(FieldTable),FieldTable^.NameLen);
        {$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
        if FieldTable^.Count=0 then begin
          {$ifndef LVCL}
          assert(FieldTable^.Size=ElemSize);
          {$endif}
          n := n*integer(ElemSize);
          move(Source^,P^,n);
          inc(Source,n);
        end else begin
          for i := 1 to n do begin
            Source := RecordLoad(P^,Source,ElemType);
            if Source=nil then
              break; // invalid record type (wrong field type)
            inc(P,ElemSize);
          end;
        end;
      end;
  end;
  // check security checksum
  if Hash32(@Hash[1],Source-PAnsiChar(@Hash[1]))<>Hash[0] then
    result := nil else
    result := Source;
end;

function TDynArray.Find(const Elem; const aIndex: TIntegerDynArray;
      aCompare: TDynArraySortCompare): integer;
var n, L, cmp: integer;
    P: PAnsiChar;
begin
  n := Count;
  if (@aCompare<>nil) and (n>0) then begin
    dec(n);
    P := Value^;
    if (n>10) and (length(aIndex)>n) then begin
      // array should be sorted -> use fast binary search
      L := 0;
      repeat
        result := (L+n) shr 1;
        cmp := aCompare(P[cardinal(aIndex[result])*ElemSize],Elem);
        if cmp=0 then
          exit;
        if cmp<0 then
          L := result+1 else
          n := result-1;
      until L>n;
    end else
      // array is not sorted -> use iterating search
      for result := 0 to n do
        if aCompare(P^,Elem)=0 then
          exit else
          inc(P,ElemSize);
  end;
  result := -1;
end;

function TDynArray.FindIndex(const Elem; aIndex: PIntegerDynArray;
  aCompare: TDynArraySortCompare): integer;
begin
  if aIndex<>nil then
    result := Find(Elem,aIndex^,aCompare) else
  if Assigned(aCompare) then
    result := Find(Elem,nil,aCompare) else
    result := Find(Elem);
end;

function TDynArray.FindAndFill(var Elem; aIndex: PIntegerDynArray=nil;
      aCompare: TDynArraySortCompare=nil): integer;
var P: PAnsiChar;
begin
  result := FindIndex(Elem,aIndex,aCompare);
  if result<0 then
    exit;
  P := PAnsiChar(Value^)+cardinal(result)*ElemSize;
  // if found, fill Elem with the matching item
  if ElemType=nil then
    move(P^,Elem,ElemSize) else
    CopyArray(@Elem,P,ElemType,1);
end;

function TDynArray.FindAndDelete(var Elem; aIndex: PIntegerDynArray=nil;
  aCompare: TDynArraySortCompare=nil): integer;
begin
  result := FindIndex(Elem,aIndex,aCompare);
  if result>=0 then
    Delete(result);
end;

function TDynArray.FindAndUpdate(const Elem; aIndex: PIntegerDynArray=nil;
  aCompare: TDynArraySortCompare=nil): integer;
var P: PAnsiChar;
begin
  result := FindIndex(Elem,aIndex,aCompare);
  if result<0 then
    exit;
  P := PAnsiChar(Value^)+cardinal(result)*ElemSize;
  // if found, fill Elem with the matching item
  if ElemType=nil then
    move(Elem,P^,ElemSize) else
    CopyArray(P,@Elem,ElemType,1);
end;

function TDynArray.FindAndAddIfNotExisting(const Elem; aIndex: PIntegerDynArray=nil;
  aCompare: TDynArraySortCompare=nil): integer;
begin
  result := FindIndex(Elem,aIndex,aCompare);
  if result<0 then
    Add(Elem); // -1 will mark success
end;

function TDynArray.Find(const Elem): integer;
var n, L, cmp: integer;
    P: PAnsiChar;
begin
  n := Count;
  if (@fCompare<>nil) and (n>0) then begin
    dec(n);
    P := Value^;
    if fSorted and (n>10) then begin
      // array is sorted -> use fast binary search
      L := 0;
      repeat
        result := (L+n) shr 1;
        cmp := fCompare(P[cardinal(result)*ElemSize],Elem);
        if cmp=0 then
          exit;
        if cmp<0 then
          L := result+1 else
          n := result-1;
      until L>n;
    end else
      // array is not sorted -> use iterating search
      for result := 0 to n do
        if fCompare(P^,Elem)=0 then
          exit else
          inc(P,ElemSize);
  end;
  result := -1;
end;

type
  // internal structure used to make QuickSort faster & with less stack usage
  TDynArrayQuickSort = {$ifndef UNICODE}object{$else}record{$endif}
    Compare: TDynArraySortCompare;
    Pivot: pointer;
    Index: PCardinalArray;
    ElemSize: cardinal;
    P: integer;
    Value: PAnsiChar;
    IP, JP: PAnsiChar;
    procedure QuickSort(L, R: PtrInt);
    procedure QuickSortIndexed(L, R: PtrInt);
  end;

procedure TDynArrayQuickSort.QuickSort(L, R: PtrInt);
var I, J, tmp: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      pivot := Value+cardinal(P)*ElemSize;
      IP := Value+cardinal(I)*ElemSize;
      JP := Value+cardinal(J)*ElemSize;
      while Compare(IP^,pivot^)<0 do begin
        inc(I);
        inc(IP,ElemSize);
      end;
      while Compare(JP^,pivot^)>0 do begin
        dec(J);
        dec(JP,ElemSize);
      end;
      if I <= J then begin
        if I<>J then
          if ElemSize=4 then begin
            // optimized version for TIntegerDynArray or TRawUTF8DynArray e.g.
            tmp := PInteger(IP)^;
            PInteger(IP)^ := PInteger(JP)^;
            PInteger(JP)^ := tmp;
          end else
            // generic exchange of row element data
            Exchg(IP,JP,ElemSize-1);
        if P = I then P := J else
        if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort(L, J);
    L := I;
  until I >= R;
end;

procedure TDynArrayQuickSort.QuickSortIndexed(L, R: PtrInt);
var I, J, tmp: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      pivot := Value+Index[P]*ElemSize;
      while Compare(Value[Index[I]*ElemSize],pivot^)<0 do inc(I);
      while Compare(Value[Index[J]*ElemSize],pivot^)>0 do dec(J);
      if I <= J then begin
        if I<>J then begin
          tmp := Index[I];
          Index[I] := Index[J];
          Index[J] := tmp;
        end;
        if P = I then P := J else
        if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSortIndexed(L, J);
    L := I;
  until I >= R;
end;

procedure TDynArray.Sort;
var QuickSort: TDynArrayQuickSort;
begin
  if (@fCompare<>nil) and (Value<>nil) and (Value^<>nil) then begin
    Quicksort.Compare := @fCompare;
    Quicksort.Value := Value^;
    Quicksort.ElemSize := ElemSize;
    Quicksort.QuickSort(0,Count-1);
    fSorted := true;
  end;
end;

procedure TDynArray.CreateOrderedIndex(var aIndex: TIntegerDynArray;
  aCompare: TDynArraySortCompare);
var QuickSort: TDynArrayQuickSort;
    n: integer;
begin
  if (@aCompare<>nil) and (Value<>nil) and (Value^<>nil) then begin
    n := Count;
    if length(aIndex)<n then begin
      SetLength(aIndex,n);
      FillIncreasing(pointer(aIndex),0,n);
    end;
    Quicksort.Compare := @aCompare;
    Quicksort.Value := Value^;
    Quicksort.ElemSize := ElemSize;
    Quicksort.Index := pointer(aIndex);
    Quicksort.QuickSortIndexed(0,n-1);
  end;
end;

function TDynArray.ElemEquals(const A,B): boolean;
begin
  if @fCompare<>nil then
    result := fCompare(A,B)=0 else
    if ElemType=nil then
      case ElemSize of // optimized versions for arrays of common types
        1: result := byte(A)=byte(B);
        2: result := word(A)=word(B);
        4: result := cardinal(A)=cardinal(B);
        8: result := Int64(A)=Int64(B);
      else result := CompareMem(@A,@B,ElemSize); // generic comparison
      end else
    case PByte(ElemType)^ of
    tkRecord:  result := RecordEquals(A,B,ElemType);
    tkLString: result := AnsiString(A)=AnsiString(B);
    tkWString: result := WideString(A)=WideString(B);
    {$ifdef UNICODE}
    tkUString: result := UnicodeString(A)=UnicodeString(B);
    {$endif}
    else result := false;
    end;
end;

{$ifndef DELPHI5OROLDER} // do not know why Delphi 5 compiler does not like it
function TDynArray.Equals(const B: TDynArray): boolean;
var i, n: integer;
    P1,P2: PAnsiChar;
    A1: PPointerArray absolute P1;
    A2: PPointerArray absolute P2;
begin
  result := false;
  if TypeInfo<>B.TypeInfo then
    exit; // array types shall match
  n := Count;
  if n<>B.Count then
    exit;
  P1 := Value^;
  P2 := B.Value^;
  if @fCompare<>nil then // if a customized comparison is available, use it
    for i := 1 to n do
      if fCompare(P1^,P2^)<>0 then
        exit else begin
        inc(P1,ElemSize);
        inc(P2,ElemSize);
      end else
  if ElemType=nil then begin // binary type is compared as a whole
    result := CompareMem(P1,P2,ElemSize*cardinal(n));
    exit;
  end else
  case PByte(ElemType)^ of
  tkRecord:
    for i := 1 to n do
      if not RecordEquals(P1^,P2^,ElemType) then
        exit else begin
        inc(P1,ElemSize);
        inc(P2,ElemSize);
      end;
  tkLString:
    for i := 0 to n-1 do
      if AnsiString(A1^[i])<>AnsiString(A2^[i]) then
        exit;
  tkWString:
    for i := 0 to n-1 do
      if WideString(A1^[i])<>WideString(A2^[i]) then
        exit;
  {$ifdef UNICODE}
  tkUString:
    for i := 0 to n-1 do
      if UnicodeString(A1^[i])<>UnicodeString(A2^[i]) then
        exit;
  {$endif}
  else exit;
  end;
  result := true;
end;
{$endif}

function TDynArray.IndexOf(const Elem): integer;
var P: pointer;
    max: integer;
begin
  if Value=nil then begin
    result := -1;
    exit; // avoid GPF if void
  end;
  max := Count-1;
  P := Value^;
  if @Elem<>nil then
  if ElemType=nil then
  case ElemSize of
    // optimized versions for arrays of byte,word,integer,Int64,Currency,Double
    1: for result := 0 to max do
         if PByteArray(P)^[result]=byte(Elem) then exit;
    2: for result := 0 to max do
         if PWordArray(P)^[result]=word(Elem) then exit;
    4: for result := 0 to max do
         if PIntegerArray(P)^[result]=integer(Elem) then exit;
    8: for result := 0 to max do // Int64,Currency,Double
         if PInt64Array(P)^[result]=Int64(Elem) then exit;
  else // generic binary comparison (fast with our overloaded CompareMem)
    for result := 0 to max do
      if CompareMem(P,@Elem,ElemSize) then
        exit else
        inc(PtrUInt(P),ElemSize);
  end else
  case PByte(ElemType)^ of
  tkLString:
    for result := 0 to max do
      if AnsiString(PPtrIntArray(P)^[result])=AnsiString(Elem) then exit;
  tkWString:
    for result := 0 to max do
      if WideString(PPtrIntArray(P)^[result])=WideString(Elem) then exit;
  {$ifdef UNICODE}
  tkUString:
    for result := 0 to max do
      if UnicodeString(PPtrIntArray(P)^[result])=UnicodeString(Elem) then exit;
  {$endif}
  tkRecord: // work with packed records containing binary and string types
    for result := 0 to max do
      if RecordEquals(P^,Elem,ElemType) then
        exit else
        inc(PtrUInt(P),ElemSize);
  end;
  result := -1;
end;

procedure TDynArray.Init(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil);
var Typ: PDynArrayTypeInfo absolute aTypeInfo;
begin
  TypeInfo := aTypeInfo;
  Value := @aValue;
  if Typ^.Kind<>tkDynArray then
    raise ESynException.CreateFmt('%s is not a dynamic array',[PShortString(@Typ^.NameLen)^]);
  inc(PtrUInt(Typ),Typ^.NameLen);
  {$ifdef FPC}Typ := AlignToPtr(Typ);{$endif}
  with Typ^ do begin
    ElemSize := elSize;
    if elType=nil then
      ElemType := nil else
      {$ifndef FPC}
      ElemType := elType^;
      {$else} begin
      if PByte(elType)^ in tkManagedTypes then
        ElemType := elType else
        ElemType := nil;
      end;
      {$endif}
  end;
  fSorted := false;
  fCompare := nil;
  fKnownType := djNone;
  fCountP := aCountPointer;
  if fCountP<>nil then
    fCountP^ := 0;
end;

procedure TDynArray.InitSpecific(aTypeInfo: pointer; var aValue; aKind: TDynArrayKind;
      aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
var Comp: TDynArraySortCompare;
begin
  Init(aTypeInfo,aValue,aCountPointer);
  Comp := SORTFIRSTFIELD[aCaseInsensitive,aKind];
  if @Comp=nil then
    raise ESynException.Create('TDynArray.InitSpecific wrong aKind');
  fCompare := Comp;
  fKnownType := aKind;
end;

procedure TDynArray.Void;
begin
  Value := nil;
end;

function TDynArray.IsVoid: boolean;
begin
  result := Value=nil;
end;

procedure _DynArrayClear(var a: Pointer; typeInfo: Pointer);
{$ifdef FPC}
  [external name 'FPC_DYNARRAY_CLEAR'];
{$else}
asm
{$ifdef CPUX64}
  .NOFRAME
{$endif}
  jmp System.@DynArrayClear
end;
{$endif}

procedure _FinalizeArray(p: Pointer; typeInfo: Pointer; elemCount: PtrUInt);
{$ifdef FPC}
  [external name 'FPC_FINALIZE_ARRAY'];
{$else}
asm
{$ifdef CPUX64}
  .NOFRAME
{$endif}
  jmp System.@FinalizeArray
end;
{$endif}

procedure TDynArray.InternalSetLength(NewLength: PtrUInt);
  procedure RangeError;
  begin
    raise ERangeError.CreateFmt('SetLength(%s,%d)',
      [PShortString(@PDynArrayTypeInfo(TypeInfo)^.NameLen)^,NewLength]);
  end;
var p: ^TDynArrayRec;
    pa: PAnsiChar absolute p;
    OldLength, NeededSize, minLength: PtrUInt;
    pp: pointer;
begin // this method is faster than default System.DynArraySetLength() function
  // check that new array length is not just a hidden finalize
  if NewLength=0 then begin
    _DynArrayClear(Value^,TypeInfo);
    exit;
  end;
  // retrieve old length
  p := Value^;
  if p<>nil then begin
    dec(PtrUInt(p),Sizeof(TDynArrayRec)); // p^ = start of heap object
    OldLength := p^.length;
  end else
    OldLength := 0;
  // calculate the needed size of the resulting memory structure on heap
  NeededSize := NewLength*ElemSize+Sizeof(TDynArrayRec);
  if NeededSize>1024*1024*512 then // max allowed memory block is 512MB
    RangeError;
  // if not shared (refCnt=1), resize; if shared, create copy (not thread safe)
  if (p=nil) or (p^.refCnt=1) then begin
    if (NewLength<OldLength) and (ElemType<>nil) then
      _FinalizeArray(pa+NeededSize,ElemType,OldLength-NewLength);
    ReallocMem(p,neededSize);
  end else begin
    Dec(p^.refCnt); 
    GetMem(p,neededSize);
    minLength := oldLength;
    if minLength>newLength then
      minLength := newLength;
    if ElemType<>nil then begin
      pp := pa+Sizeof(TDynArrayRec);
      FillChar(pp^,minLength*elemSize,0);
      CopyArray(pp,Value^,ElemType,minLength)
    end else
      Move(Value^,pa[Sizeof(TDynArrayRec)],minLength*elemSize);
  end;
  // set refCnt=1 and new length to the heap memory structure
  with p^ do begin
    refCnt := 1;
    {$ifdef FPC}
    high := newLength-1;
    {$else}
    length := newLength;
    {$endif}
  end;
  Inc(PtrUInt(p),Sizeof(p^));
  // reset new allocated elements content to zero 
  OldLength := OldLength*elemSize;
  FillChar(pa[OldLength],neededSize-OldLength-Sizeof(TDynArrayRec),0);
  Value^ := p;
end;

procedure TDynArray.SetCount(aCount: integer);
var capa, delta: integer;
begin
  fSorted := false;
  if Value=nil then
    exit; // avoid GPF if void
  if fCountP<>nil then begin
    delta := aCount-fCountP^;
    if delta=0 then
      exit;
    fCountP^ := aCount;
    if PtrInt(Value^)=0 then begin
      // no capa yet
      if (delta>0) and (aCount<64) then
        aCount := 64; // reserve some minimal space for Add()
    end else begin
      capa := PInteger(PtrInt(Value^)-sizeof(Integer))^;
      if delta>0 then begin
        // size-up -> grow by chunks
        if capa>=fCountP^ then
          exit; // no need to grow
        Inc(capa,capa shr 2);
        if capa<fCountP^ then
          aCount := fCountP^ else
          aCount := capa;
      end else
        // size-down -> check if worth it (for faster Delete)
        if capa-aCount<capa shr 3 then
          exit;
    end;
  end;
  // no external Count, array size-down or array up-grow -> realloc
  InternalSetLength(aCount);
end;

function TDynArray.GetCapacity: integer;
begin // capacity := length(DynArray)
  if (Value<>nil) and (PtrInt(Value^)<>0) then
    result := PInteger(PtrInt(Value^)-sizeof(Integer))^ else
    result := 0;
end;

procedure TDynArray.SetCapacity(aCapacity: integer);
begin
  if Value=nil then
    exit; // avoid GPF if void
  if fCountP<>nil then
    if fCountP^>aCapacity then
      fCountP^ := aCapacity;
  InternalSetLength(aCapacity);
end;

procedure TDynArray.SetCompare(const aCompare: TDynArraySortCompare);
begin
  if @aCompare<>@fCompare then begin
    @fCompare := @aCompare;
    fSorted := false;
  end;
end;

procedure TDynArray.Copy(Source: TDynArray);
var n: Cardinal;
begin
  if (Value=nil) or (Source.Value=nil) or (TypeInfo<>Source.TypeInfo) then
    Exit;
  n := Source.Count;
  SetCount(n);
  if ElemType=nil then
    move(Source.Value^^,Value^^,n*ElemSize) else
    CopyArray(Source.Value^,Value^,TypeInfo,n);
end;

procedure TDynArray.Slice(var Dest; aCount: Cardinal; aFirstIndex: cardinal=0);
var n: Cardinal;
    D: PPointer;
    P: PAnsiChar;
begin
  if Value=nil then
    exit; // avoid GPF if void
  n := Count;
  if aFirstIndex>=n then
    aCount := 0 else
  if aCount>=n-aFirstIndex then
    aCount := n-aFirstIndex;
  DynArray(TypeInfo,Dest).InternalSetLength(aCount);
  D := @Dest;
  if aCount>0 then begin
    P := PAnsiChar(Value^)+aFirstIndex*ElemSize;
    if ElemType=nil then
      move(P^,D^^,aCount*ElemSize) else
      CopyArray(D^,P,TypeInfo,aCount);
  end;
end;

procedure TDynArray.AddArray(const DynArray; aStartIndex: integer=0; aCount: integer=-1);
var n: integer;
    D: PPointer;
    PS,PD: pointer;
begin
  if Value=nil then
    exit; // avoid GPF if void
  D := @DynArray;
  if D^=nil then
    n := 0 else
    n := PInteger(PtrUInt(D^)-sizeof(Integer))^;
  if aStartIndex>=n then
    exit; // nothing to copy
  if cardinal(aStartIndex+aCount)>cardinal(n) then
    aCount := n-aStartIndex;
  if aCount<=0 then
    exit;
  n := Count;
  SetCount(n+aCount);
  PS := pointer(PtrUInt(D^)+cardinal(aStartIndex)*ElemSize);
  PD := pointer(PtrUInt(Value^)+cardinal(n)*ElemSize);
  if ElemType=nil then
    move(PS^,PD^,cardinal(aCount)*ElemSize) else
    CopyArray(PD,PS,TypeInfo,aCount);
end;

procedure TDynArray.ElemClear(var Elem);
begin
  if ElemType<>nil then
    case PByte(ElemType)^ of // release reference counted
      tkLString: RawByteString(Elem) := '';
      tkWString: WideString(Elem) := '';
      {$ifdef UNICODE}tkUString: UnicodeString(Elem) := ''; {$endif}
      tkRecord:  RecordClear(Elem,ElemType);
      else exit; 
    end;
  fillchar(Elem,ElemSize,0); // always fill with zero binary content
end;

procedure TDynArray.ElemCopy(const A; var B);
begin
  if ElemType=nil then
    move(A,B,ElemSize) else
    CopyArray(@B,@A,ElemType,1);
end;

function TDynArray.ElemLoad(Source: PAnsiChar): RawByteString;
begin
  if (Source<>nil) and (ElemType=nil) then
    SetString(result,Source,ElemSize) else begin
    SetString(result,nil,ElemSize);
    FillChar(pointer(result)^,ElemSize,0);
    ElemLoad(Source,pointer(result)^);
  end;
end;

procedure TDynArray.ElemLoad(Source: PAnsiChar; var Elem);
begin
  if Source=nil then
    exit; // avoid GPF
  if ElemType=nil then
    move(Source^,Elem,ElemSize) else
    case PByte(ElemType)^ of
    tkLString: begin
      SetString(RawByteString(Elem),Source+4,PInteger(Source)^);
      {$ifdef UNICODE}
      { Delphi 2009+: set Code page for this AnsiString }
      if PPtrUInt(@Elem)^<>0 then
        PWord(PPtrUInt(@Elem)^-12)^ := PWord(PtrUInt(ElemType)+
          PDynArrayTypeInfo(ElemType)^.NameLen+2)^;
      {$endif}
    end;
    tkWString:
      SetString(WideString(Elem),PWideChar(Source+4),PInteger(Source)^);
    {$ifdef UNICODE}
    tkUString:
      SetString(UnicodeString(Elem),PWideChar(Source+4),PInteger(Source)^);
    {$endif}
    tkRecord:
      RecordLoad(Elem,Source,ElemType);
    end;
end;

procedure TDynArray.ElemLoadClear(var ElemLoaded: RawByteString);
begin
  if (ElemType<>nil) and (length(ElemLoaded)=integer(ElemSize)) then
  case PByte(ElemType)^ of
    tkLString: PRawByteString(pointer(ElemLoaded))^ := '';
    tkWString: PWideString(pointer(ElemLoaded))^ := '';
    {$ifdef UNICODE}tkUString: PUnicodeString(pointer(ElemLoaded))^ := ''; {$endif}
    tkRecord: RecordClear(pointer(ElemLoaded)^,ElemType);
  end;
  ElemLoaded := '';
end;

function TDynArray.ElemSave(const Elem): RawByteString;
begin
  if ElemType=nil then
    SetString(result,PAnsiChar(@Elem),ElemSize) else
    case PByte(ElemType)^ of
      tkLString:
      if PPtrInt(@Elem)^=0 then
        SetString(result,PAnsiChar(@Elem),4) else
        SetString(result,PAnsiChar(PPtrInt(@Elem)^-4),PInteger(PPtrInt(@Elem)^-4)^+4);
      tkWString{$ifdef UNICODE} ,tkUString{$endif}:
      if PPtrInt(@Elem)^=0 then
        SetString(result,PAnsiChar(@Elem),4) else
        SetString(result,PAnsiChar(PPtrInt(@Elem)^-4),PInteger(PPtrInt(@Elem)^-4)^*2+4);
      tkRecord:
        result := RecordSave(Elem,ElemType);
      else result := '';
    end;
end;

function TDynArray.ElemLoadFind(Source: PAnsiChar): integer;
var tmp: RawByteString;
begin
  tmp := ElemLoad(Source);
  if tmp='' then
    result := -1 else
    try
      if @fCompare=nil then
        result := IndexOf(pointer(tmp)^) else
        result := Find(pointer(tmp)^);
    finally
      ElemLoadClear(tmp);
    end;
end;

function DynArray(aTypeInfo: pointer; var aValue; aCountPointer: PInteger=nil): TDynArray;
begin
  result.Init(aTypeInfo,aValue,aCountPointer);
end;


{ TDynArrayHashed }

function TDynArrayHashed.FindHashed(const Elem): integer;
begin
  if fHashs=nil then
    result := -1 else begin
    result := HashFind(HashOne(Elem),Elem);
    if result<0 then
      result := -1; // for coherency with most methods
  end;
end;

procedure TDynArrayHashed.HashAdd(const Elem; aHashCode: Cardinal; var result: integer);
var n: integer;
begin
  n := Count;
  SetCount(n+1); // reserve space for a void element in array
  if Capacity+1>=length(fHashs) then begin
    // fHashs[] is too small -> recreate
    ReHash;
    result := HashFind(aHashCode,Elem); // fHashs[] has changed -> recompute
    assert(result<0);
  end;
  with fHashs[-result-1] do begin // HashFind returned negative index in fHashs[]
    Hash := aHashCode;
    Index := n;
  end;
  result := n;
end;

function TDynArrayHashed.FindHashedForAdding(const Elem; out wasAdded: boolean;
  aHashCode: cardinal): integer;
begin
  if aHashCode=0 then
    aHashCode := HashOne(Elem);
  result := HashFind(aHashCode,Elem);
  if result>=0 then
    // found matching existing item
    wasAdded := false else begin
    // create a void element
    HashAdd(Elem,aHashCode,result);
    wasAdded := true;
  end;
end;

function TDynArrayHashed.AddAndMakeUniqueName(aName: RawUTF8): pointer;
var c,j: integer;
    added: boolean;
    aName_: RawUTF8;
begin
  c := FindHashedForAdding(aName,added);
  if not added then begin // force unique column name
    aName_ := aName+'_';
    j := 1;
    repeat
      aName := aName_+UInt32ToUTF8(j);
      c := FindHashedForAdding(aName,added);
      inc(j);
    until added;
  end;
  assert(c=Count-1);
  result := PAnsiChar(Value^)+cardinal(c)*ElemSize;
  PRawUTF8(result)^ := aName; // store unique name at 1st elem position
end;

function TDynArrayHashed.FindHashedAndFill(var Elem): integer;
var P: PAnsiChar;
begin
  result := HashFind(HashOne(Elem),Elem);
  if result<0 then
    result := -1 else begin
    // copy from dynamic array found entry into Elem = Fill
    P := PAnsiChar(Value^)+cardinal(result)*ElemSize;
    if ElemType=nil then
      move(P^,Elem,ElemSize) else
      CopyArray(@Elem,P,ElemType,1);
  end;
end;

function TDynArrayHashed.FindHashedAndUpdate(var Elem; AddIfNotExisting: boolean): integer;
var P: PAnsiChar;
    aHashCode: cardinal;
begin
  aHashCode := HashOne(Elem);
  result := HashFind(aHashCode,Elem);
  if result<0 then
    if AddIfNotExisting then begin
      // not existing -> add as new element
      HashAdd(Elem,aHashCode,result); // ReHash only if necessary
      P := PAnsiChar(Value^)+cardinal(result)*ElemSize;
      if ElemType=nil then
        move(Elem,P^,ElemSize) else
        CopyArray(P,@Elem,ElemType,1);
    end else
      result := -1 else begin
    // copy from Elem into dynamic array found entry = Update
    P := PAnsiChar(Value^)+cardinal(result)*ElemSize;
    if ElemType=nil then
      move(Elem,P^,ElemSize) else
      CopyArray(P,@Elem,ElemType,1);
    ReHash;
  end;
end;

function TDynArrayHashed.FindHashedAndDelete(var Elem): integer;
begin
  result := HashFind(HashOne(Elem),Elem);
  if result<0 then
    result := -1 else begin
    Delete(result);
    ReHash;
  end;
end;

function HashAnsiString(const Elem; Hasher: THasher): cardinal;
begin
  if PtrUInt(Elem)=0 then
    result := 1 else
    result := Hasher(0,Pointer(PtrUInt(Elem)),PInteger(PtrUInt(Elem)-4)^);
end;

function HashUnicodeString(const Elem; Hasher: THasher): cardinal;
begin
  if PtrUInt(Elem)=0 then
    result := 1 else
    result := Hasher(0,Pointer(PtrUInt(Elem)),PInteger(PtrUInt(Elem)-4)^*2);
end;

function HashAnsiStringI(const Elem; Hasher: THasher): cardinal;
var tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Elem)=0 then
    result := 1 else
    result := Hasher(0,tmp,UpperCopy255(tmp,RawUTF8(Elem))-tmp);
end;

function HashUnicodeStringI(const Elem; Hasher: THasher): cardinal;
var tmp: array[byte] of AnsiChar; // avoid slow heap allocation
begin
  if PtrUInt(Elem)=0 then
    result := 1 else
    result := Hasher(0,tmp,UpperCopy255W(tmp,SynUnicode(Elem))-tmp);
end;

function HashPtrUInt(const Elem; Hasher: THasher): cardinal;
begin
{$ifdef CPU64}
  result := Hasher(0,@Elem,sizeof(Int64)); // better than Int64Rec.(Lo xor Hi)
{$else}
  result := PtrUInt(Elem);
{$endif}
end;

function HashByte(const Elem; Hasher: THasher): cardinal;
begin
  result := Byte(Elem);
end;

function HashWord(const Elem; Hasher: THasher): cardinal;
begin
  result := Word(Elem);
end;

function HashInteger(const Elem; Hasher: THasher): cardinal;
begin
  result := Integer(Elem);
end;

function HashCardinal(const Elem; Hasher: THasher): cardinal;
begin
  result := Cardinal(Elem);
end;

function HashInt64(const Elem; Hasher: THasher): cardinal;
begin
  result := Hasher(0,@Elem,sizeof(Int64)); // better than Int64Rec.(Lo xor Hi)
end;

const
  HASHFIRSTFIELD: array[boolean,TDynArrayKind] of TDynArrayHashOne = (
    (nil, HashByte, HashWord, HashInteger,
    HashCardinal, HashInt64, HashInt64,
    HashInt64, HashInt64, HashInt64,
    HashAnsiString, HashAnsiString,
    {$ifdef UNICODE}HashUnicodeString{$else}HashAnsiString{$endif},
    HashUnicodeString, HashUnicodeString, nil),
    (nil, HashByte, HashWord, HashInteger,
    HashCardinal, HashInt64, HashInt64,
    HashInt64, HashInt64, HashInt64,
    HashAnsiStringI, HashAnsiStringI,
    {$ifdef UNICODE}HashUnicodeStringI{$else}HashAnsiStringI{$endif},
    HashUnicodeStringI, HashUnicodeStringI, nil));

procedure TDynArrayHashed.InitSpecific(aTypeInfo: pointer; var aValue;
  aKind: TDynArrayKind; aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
var Comp: TDynArraySortCompare;
    Hasher: TDynArrayHashOne;
begin
  Comp := SORTFIRSTFIELD[aCaseInsensitive,aKind];
  Hasher := HASHFIRSTFIELD[aCaseInsensitive,aKind];
  if (@Hasher=nil) or (@Comp=nil) then
    raise ESynException.Create('TDynArrayHashed.InitSpecific wrong aKind');
  Init(aTypeInfo,aValue,Hasher,Comp,nil,aCountPointer,aCaseInsensitive);
  fKnownType := aKind;
end;

procedure TDynArrayHashed.Init(aTypeInfo: pointer; var aValue;
      aHashElement: TDynArrayHashOne=nil; aCompare: TDynArraySortCompare=nil;
      aHasher: THasher=nil; aCountPointer: PInteger=nil; aCaseInsensitive: boolean=false);
var aKind: TDynArrayKind;
begin
  inherited Init(aTypeInfo,aValue,aCountPointer);
  fEventCompare := nil;
  if @aHasher=nil then begin
    if @DefaultHasher=nil then
      DefaultHasher := @kr32; // set here so that kr32() could be smart-linked
    fHasher := DefaultHasher;
  end else
    fHasher := aHasher;
  if (@aHashElement=nil) or (@aCompare=nil) then begin
    // it's faster to retrieve now the hashing/compare function than in HashOne
    aKind := ToKnownType;
    if @aHashElement=nil then
      aHashElement := HASHFIRSTFIELD[aCaseInsensitive,aKind];
    if @aCompare=nil then
      aCompare := SORTFIRSTFIELD[aCaseInsensitive,aKind];
  end;
  fHashElement := aHashElement;
  fCompare := aCompare;
  SetLength(fHashs,0);
end;

procedure TDynArrayHashed.HashInit;
var cap, PO2: integer;
begin
  SetLength(fHashs,0); // any previous hash is invalid
  // find nearest power of two for new fHashs[] size
  cap := Capacity+64; // use Capacity instead of Count (avoid ReHash calls)
  PO2 := 256;       // + 64 to increase number of void entries (for speed)
  while PO2<cap do
    PO2 := PO2 shl 1;
  SetLength(fHashs,PO2);
end;

function TDynArrayHashed.HashFind(aHashCode: cardinal; const Elem): integer;
var n, first: integer;
    looped: boolean;
begin
  looped := false;
  if fHashs=nil then
    HashInit;
  n := length(fHashs);
  result := (aHashCode-1) and (n-1); // fHashs[] has a power of 2 length
  first := result;
  repeat
    with fHashs[result] do
    if Hash=aHashCode then
      if not Assigned(fEventCompare) then
        if @fCompare<>nil then begin
          if fCompare(PAnsiChar(Value^)[Index*ElemSize],Elem)=0 then begin
            result := Index;
            exit; // found -> returns index in dynamic array
          end;
        end else begin
        if ElemEquals(PAnsiChar(Value^)[Index*ElemSize],Elem) then begin
          result := Index;
          exit; // found -> returns index in dynamic array
        end;
      end else begin
        if fEventCompare(PAnsiChar(Value^)[Index*ElemSize],Elem)=0 then begin
          result := Index;
          exit; // found -> returns index in dynamic array
        end;
      end else
    if Hash=0 then begin
      result := -(result+1);
      exit; // not found -> returns void index in fHashs[] as negative
    end;
    // hash colision -> search next item
    inc(result);
    if result=n then
      // reached the end -> search once from fHash[0] to fHash[first-1]
      if looped then
        Break else begin
        result := 0;
        n := first;
        looped := true;
      end;
  until false;
  raise ESynException.Create('HashFind'); // we should never reach here
end;

function TDynArrayHashed.GetHashFromIndex(aIndex: Integer): Cardinal;
begin
  if cardinal(aIndex)>=cardinal(Count) then
    result := 0 else
    // it's faster to rehash than to loop in fHashs[].Index values
    result := HashOne(PAnsiChar(Value^)[cardinal(aIndex)*ElemSize]);
end;

function TDynArrayHashed.HashOne(const Elem): cardinal;
var FieldTable: PFieldTable;
label Bin, LStr, WStr, Rec;
begin
  if @fHashElement<>nil then
    result := fHashElement(Elem,fHasher) else
  if ElemType=nil then
    goto Bin else
    case PByte(ElemType)^ of // Init() should have already set the good function
    tkLString:
LStr: if PtrUInt(Elem)=0 then
        result := 0 else
        result := fHasher(0,Pointer(PtrUInt(Elem)),PInteger(PtrUInt(Elem)-4)^);
    tkWString{$ifdef UNICODE}, tkUString{$endif}:
WStr: if PtrUInt(Elem)=0 then
        result := 0 else
        result := fHasher(0,Pointer(PtrUInt(Elem)),PInteger(PtrUInt(Elem)-4)^*2);
    tkRecord: begin
      FieldTable := ElemType;
Rec:  inc(PtrUInt(FieldTable),FieldTable^.NameLen);
{$ifdef FPC}FieldTable := AlignToPtr(FieldTable);{$endif}
      if FieldTable^.Count=0 then
        // only binary content -> hash full content
Bin:    case ElemSize of
          1: result := byte(Elem);
          2: result := word(Elem);
          4: result := cardinal(Elem);
          8: result := Int64Rec(Elem).Lo xor Int64Rec(Elem).Hi;
        else result := fHasher(0,@Elem,ElemSize);
        end else
      with FieldTable^.Fields[0] do
      if Offset<>0 then
        // hash whole starting binary content of record
        result := fHasher(0,@Elem,Offset) else
        // hash first string field of record
        case TypeInfo^^.Kind of
        tkLString: goto LStr;
        tkWString{$ifdef UNICODE}, tkUString{$endif}: goto WStr;
        tkRecord: begin
          FieldTable := pointer(TypeInfo^);
          goto Rec;
        end;
        else result := 0; // unhandled type
        end;
    end;
    else result := 0;
    end;
  if result=0 then
    result := 1; // never return 0 (indicates a free position in fHashs[])
end;

procedure TDynArrayHashed.ReHash(aHasher: TOnDynArrayHashOne=nil);
var i, n, ndx: integer;
    P: PAnsiChar;
    aHashCode: cardinal;
begin
  HashInit;
  n := Count;
  if n>0 then begin // avoid GPF after TDynArray.Clear call (Count=0)
    P := Value^;
    for i := 0 to n-1 do begin
      if @aHasher=nil then
        if @fHashElement<>nil then
          aHashCode := fHashElement(P^,fHasher) else
          aHashCode := HashOne(P^) else
        aHashCode := aHasher(P^);
      ndx := HashFind(aHashCode,P^);
      if ndx<0 then
        // >=0 -> already found -> not necessary to add duplicated hash
        with fHashs[-ndx-1] do begin
          Hash := aHashCode;
          Index := i;
        end;
      inc(P,ElemSize);
    end;
  end;
end;



{ TObjectHash }

const
  COUNT_TO_START_HASHING = 32;

function TObjectHash.Find(Item: TObject): integer;
var n: integer;
begin
  n := Count;
  if n<=COUNT_TO_START_HASHING then begin
    for result := 0 to n-1 do // loop comparison if not worth it
      if Compare(Get(result),Item) then
        exit;
    result := -1;
    exit;
  end;
  result := HashFind(Hash(Item),Item);
end;

function TObjectHash.HashFind(aHashCode: cardinal; Item: TObject): integer;
var n, first: integer;
    looped: boolean;
begin
  looped := false;
  if fHashs=nil then
    HashInit(Count);
  n := length(fHashs);
  result := (aHashCode-1) and (n-1); // fHashs[] has a power of 2 length
  first := result;
  repeat
    with fHashs[result] do
    if Hash=aHashCode then begin
      if Compare(Get(Index),Item) then begin
        result := Index;
        exit; // found -> returns index in list
      end;
    end else
    if Hash=0 then begin
      result := -(result+1);
      exit; // not found -> returns void index in fHashs[] as negative
    end;
    // hash colision -> search next item
    inc(result);
    if result=n then
      // reached the end -> search once from fHash[0] to fHash[first-1]
      if looped then
        Break else begin
        result := 0;
        n := first;
        looped := true;
      end;
  until false;
  raise ESynException.Create('HashFind'); // we should never reach here
end;

procedure TObjectHash.HashInit(aCountToHash: integer);
var n,PO2,i,ndx: integer;
    H: cardinal;
    O: TObject;
begin
  assert(fHashs=nil);
  // find nearest power of two for new fHashs[] size
  n := aCountToHash+64+aCountToHash shr 3; // create some void entries (for speed)
  PO2 := 256;
  while PO2<n do
    PO2 := PO2 shl 1;
  SetLength(fHashs,PO2);
  // hash all items
  for i := 0 to aCountToHash-1 do begin
    O := Get(i);
    H := Hash(O);
    ndx := HashFind(H,O);
    if ndx>=0 then
      raise ESynException.Create('HashInit found dup');
    with fHashs[-ndx-1] do begin
      Hash := H;
      Index := i;
    end;
  end;
end;

procedure TObjectHash.Invalidate;
begin
  SetLength(fHashs,0); // force HashInit call on next Find()
end;

function TObjectHash.JustAdded: boolean;
var H: cardinal;
    n,ndx: integer;
    O: TObject;
begin
  n := Count-1;
  O := Get(n);
  if O=nil then
    raise ESynException.Create('Invalid JustAdded call');
  if n<COUNT_TO_START_HASHING then begin
    result := false;
    for ndx := 0 to n-1 do // loop comparison if not worth it
      if Compare(Get(ndx),O) then
        exit;
    result := true;
    exit;
  end;
  if n+32+n shr 3>length(fHashs) then begin
    SetLength(fHashs,0);
    HashInit(n); // re-compute fHashs up to Count-1 if not enough void positions
  end;
  H := Hash(O);
  ndx := HashFind(H,O);
  if ndx>=0 then begin
    result := true; // duplicate found
    exit;
  end;
  with fHashs[-ndx-1] do begin
    Hash := H;
    Index := n;
  end;
  result := true;
end;


{ ****************** text buffer and JSON functions and classes ********* }

{ TTextWriter }

procedure TTextWriter.Add(Value: integer);
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrInt32(@tmp[15],Value);
  AddNoJSONEscape(P,@tmp[15]-P);
end;

procedure TTextWriter.AddCurr64(Value: PInt64);
begin
  if B+31>=BEnd then
    FlushInc else
    inc(B);
  inc(B,Curr64ToPChar(Value^,B)-1);
end;

procedure TTextWriter.AddCurr64(const Value: Int64);
begin
  if B+31>=BEnd then
    FlushInc else
    inc(B);
  inc(B,Curr64ToPChar(Value,B)-1);
end;

procedure TTextWriter.AddCurr64(const Value: currency);
begin
  if B+31>=BEnd then
    FlushInc else
    inc(B);
  inc(B,Curr64ToPChar(PInt64(@Value)^,B)-1);
end;

procedure TTextWriter.AddTimeLog(Value: PInt64);
begin
  if B+31>=BEnd then
    FlushInc else
    inc(B);
  inc(B,PIso8601(Value)^.Text(B,true,'T')-1);
end;

procedure TTextWriter.AddDateTime(Value: PDateTime; FirstChar: AnsiChar; QuoteChar: AnsiChar);
begin
  if Value^=0 then
    exit;
  if B+21>=BEnd then
    FlushInc else
    inc(B);
  if QuoteChar<>#0 then
    B^ := QuoteChar else
    dec(B);
  if Value^<>0 then begin
    inc(B);
    if trunc(Value^)<>0 then begin
      DateToIso8601PChar(Value^,B,true);
      inc(B,10);
    end;
    if frac(Value^)<>0 then begin
      TimeToIso8601PChar(Value^,B,true,FirstChar);
      inc(B,9);
    end;
    dec(B);
  end;
  if QuoteChar<>#0 then begin
    inc(B);
    B^ := QuoteChar;
  end;
end;

procedure TTextWriter.AddDateTime(const Value: TDateTime);
begin
  if Value=0 then
    exit;
  if B+19>=BEnd then
    FlushInc else
    inc(B);
  if trunc(Value)<>0 then begin
    DateToIso8601PChar(Value,B,true);
    inc(B,10);
  end;
  if frac(Value)<>0 then begin
    TimeToIso8601PChar(Value,B,true,'T');
    inc(B,9);
  end;
  dec(B);
end;

procedure TTextWriter.AddU(Value: cardinal);
var tmp: array[0..15] of AnsiChar;
    P: PAnsiChar;
begin
  P := StrUInt32(@tmp[15],Value);
  AddNoJSONEscape(P,@tmp[15]-P);
end;

procedure TTextWriter.Add(Value: double);
var S: ShortString;
begin
  if Value=0 then
    Add('0') else
    AddNoJSONEscape(@S[1],ExtendedToString(S,Value,DOUBLE_PRECISION));
end;

procedure TTextWriter.Add(Value: Int64);
var tmp: array[0..23] of AnsiChar;
    P: PAnsiChar;
begin
{$ifdef CPU64}
  P := StrInt32(@tmp[23],Value); // StrInt32 use PtrInt, i.e. Int64
{$else}
  P := StrInt64(@tmp[23],Value);
{$endif}
  AddNoJSONEscape(P,@tmp[23]-P);
end;

procedure TTextWriter.AddFloatStr(P: PUTF8Char);
var L: cardinal;
begin
  L := StrLen(P);
  assert(L<29);
  if L=0 then
    Add('0') else begin
    if B+31>=BEnd then
      FlushInc else
      inc(B);
    if PWord(P)^=ord('-')+ord('.')shl 8 then begin
      PWord(B)^ := ord('-')+ord('.')shl 8; // '-.3' -> '-0.3'
      inc(B,2);
      inc(P);
      dec(L);
    end else
    if P^='.' then begin
      B^ := '0'; // '.5' -> '0.5'
      inc(B);
    end;
    move(P^,B^,L);
    inc(B,L-1);
  end;
end;

procedure TTextWriter.Add(c: AnsiChar);
begin
  if B>=BEnd then
    FlushInc^ := c else
  begin
    B[1] := c;
    inc(B);
  end;
end;

procedure TTextWriter.Add(c1, c2: AnsiChar);
begin
  if B+1>=BEnd then
    FlushInc else
    inc(B);
  B^ := c1;
  inc(B);
  B^ := c2;
end;

procedure TTextWriter.AddCR;
begin
  if B+1>=BEnd then
    FlushInc
  else
    inc(B);
  pWord(B)^ := 13+10 shl 8; // CR + LF
  inc(B);
end;

procedure TTextWriter.AddChars(aChar: AnsiChar; aCount: integer);
begin
  if cardinal(aCount-1)>=cardinal(fTempBufSize) then
    exit;
  // inc(B) is done first to allow CancelLastChar
  if B+aCount>=BEnd then
    FlushInc
  else
    inc(B);
  fillchar(B^,aCount,ord(aChar));
  inc(B,aCount-1);
end;

procedure TTextWriter.Add2(Value: integer);
begin
  if B+3>=BEnd then
    FlushInc
  else
    inc(B);
  if cardinal(Value)>99 then
    pCardinal(B)^ := $3030+ord(',')shl 16
  else     // '00,' if overflow
    pCardinal(B)^ := TwoDigitLookupW[Value]+ord(',')shl 16;
  inc(B,2);
end;

procedure TTextWriter.Add4(Value: integer);
begin
  if B+5>=BEnd then
    FlushInc else
    inc(B);
  if cardinal(Value)>9999 then
    pCardinal(B)^ := $30303030
  else // '0000,' if overflow
    YearToPChar(Value,B);
  inc(B,4);
  B^ := ',';
end;

var // can be safely made global since timing is multi-thread safe
  GlobalTime: TSystemTime;
  GlobalClock: cardinal;

procedure TTextWriter.AddCurrentLogTime;
var Ticks: cardinal;
begin
  if B+17>=BEnd then
    FlushInc
  else
    inc(B);
  Ticks := GetTickCount; // this call is very fast (just one integer mul)
  if GlobalClock<>Ticks then
  begin
    GlobalClock := Ticks; // typically in range of 10-16 ms
    GetLocalTime(GlobalTime); // avoid slower API call
  end;
  YearToPChar(GlobalTime.wYear,B);
  with GlobalTime do
  begin
    PWord(B+4)^ := TwoDigitLookupW[wMonth];
    PWord(B+6)^ := TwoDigitLookupW[wDay];
    B[8] := ' ';
    PWord(B+9)^ := TwoDigitLookupW[wHour];
    PWord(B+11)^ := TwoDigitLookupW[wMinute];
    PWord(B+13)^ := TwoDigitLookupW[wSecond];
    PWord(B+15)^ := TwoDigitLookupW[wMilliseconds shr 4]; // range 0..62 = 16 ms
    //PWord(B+17)^ := TwoDigitLookupW[Byte(wMilliseconds)];
    B[17] := ' ';
  end;
  inc(B,16);
end;

procedure TTextWriter.AddMicroSec(MS: cardinal);
function Value3Digits(V: Cardinal; P: PUTF8Char): Cardinal;
begin
  PWord(P+1)^ := TwoDigitLookupW[V mod 100];
  P^ := AnsiChar((V div 100)mod 10+48);
  result := V div 1000;
end;
begin // 00.000.000
  if B+17>=BEnd then
    FlushInc
  else
    inc(B);
  B[2] := '.';
  B[6] := '.';
  MS := Value3Digits(Value3Digits(MS,B+7),B+3);
  if MS>100 then
    MS := 99;
  PWord(B)^:= TwoDigitLookupW[MS];
  inc(B,9);
end;

procedure TTextWriter.Add3(Value: integer);
begin
  if B+4>=BEnd then
    FlushInc else
    inc(B);
  if cardinal(Value)>999 then
    pCardinal(B)^ := $303030
  else // '0000,' if overflow
    pCardinal(B)^ := TwoDigitLookupW[Value div 10]+
      ord(Value mod 10+48)shl 16;
  inc(B,3);
  B^ := ',';
end;

procedure TTextWriter.AddCSV(const Integers: array of Integer);
var i: integer;
begin
  if length(Integers)=0 then
    exit;
  for i := 0 to high(Integers) do
  begin
    Add(Integers[i]);
    Add(',');
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddCSV(const Doubles: array of double);
var i: integer;
begin
  if length(Doubles)=0 then
    exit;
  for i := 0 to high(Doubles) do
  begin
    Add(Doubles[i]);
    Add(',');
  end;
  CancelLastComma;
end;

procedure TTextWriter.AddCSV(const Values: array of RawUTF8);
var i: integer;
begin
  if length(Values)=0 then
    exit;
  for i := 0 to high(Values) do begin
    Add('"');
    AddJSONEscape(pointer(Values[i]));
    Add('"',',');
  end;
  CancelLastComma;
end;

procedure TTextWriter.WriteObject(Value: TObject; HumanReadable: boolean=false;
  DontStoreDefault: boolean=true; FullExpand: boolean=false);
var i: integer;
begin
  if Value<>nil then
    if Value.InheritsFrom(TRawUTF8List) then
    with TRawUTF8List(Value) do
    begin
      self.Add('[');
      for i := 0 to Count-1 do begin
        self.Add('"');
        self.AddJSONEscape(pointer(fList[i]));
        self.Add('"',',');
      end;
      self.CancelLastComma;
      self.Add(']');
      exit;
    end else
    if Value.InheritsFrom(TStrings) then
    with TStrings(Value) do
    begin
      self.Add('[');
      for i := 0 to Count-1 do begin
        self.Add('"');
        self.AddJSONEscapeString(Strings[i]);
        self.Add('"',',');
      end;
      self.CancelLastComma;
      self.Add(']');
      exit;
    end else
    if not FullExpand or
       not (Value.InheritsFrom(TList)
       {$ifndef LVCL} or Value.InheritsFrom(TCollection){$endif}) then
      Value := nil;
  if Value=nil then begin
    AddShort('null');
    exit;
  end;
  Add('{');
  AddInstanceName(Value,':');
  Add('[');
  if Value.InheritsFrom(TList) then
    for i := 0 to TList(Value).Count-1 do
      AddInstanceName(TList(Value).List[i],',')
    {$ifndef LVCL} else
    for i := 0 to TCollection(Value).Count-1 do
      AddInstanceName(TCollection(Value).Items[i],',') {$endif} ;
  CancelLastComma;
  Add(']','}');
end;

class procedure TTextWriter.RegisterCustomJSONSerializer(aTypeInfo: pointer;
  aReader: TDynArrayJSONCustomReader; aWriter: TDynArrayJSONCustomWriter);
var i: integer;
    DynArrayTI, RecordTI: pointer;
begin
  i := -2;
  RecordTI := nil;
  DynArrayTI := nil;
  if aTypeInfo<>nil then
  case PDynArrayTypeInfo(aTypeInfo)^.kind of
  tkDynArray: begin
    DynArrayTI := aTypeInfo;
    RecordTI := TypeInfoToRecordInfo(aTypeInfo);
    i := JSONCustomParsersDynArrayIndex(aTypeInfo);
    if (i<0) and (RecordTI<>nil) then
      i := JSONCustomParsersRecordIndex(RecordTI);
    end;
  tkRecord: begin
    i := JSONCustomParsersRecordIndex(aTypeInfo);
    RecordTI := aTypeInfo;
    DynArrayTI := nil;
  end;
  end;
  if i=-2 then
    raise ESynException.Create('Invalid TTextWriter.RegisterCustomJSONSerializer call');
  if i<0 then
    if Assigned(aWriter) or Assigned(aReader) then begin
      i := length(JSONCustomParsers);
      SetLength(JSONCustomParsers,i+1);
    end else
    exit;
  with JSONCustomParsers[i] do
  begin
    DynArrayTypeInfo := DynArrayTI;
    RecordTypeInfo := RecordTI;
    Writer := aWriter;
    Reader := aReader;
  end;
end;

procedure TTextWriter.SetStream(const Value: TStream);
begin
  fStream := Value;
  fInitialStreamPosition := fStream.Seek(0,soFromCurrent);
end;

procedure TTextWriter.AddRecordJSON(const Rec; TypeInfo: pointer);
var custom: integer;
begin
  if (self=nil) or (@Rec=nil) or (TypeInfo=nil) or
     (PDynArrayTypeInfo(TypeInfo)^.kind<>tkRecord) then
    raise ESynException.Create('Invalid TTextWriter.AddRecordJSON call');
  custom := JSONCustomParsersRecordIndex(TypeInfo);
  if (custom<0) or not Assigned(JSONCustomParsers[custom].Writer) then
    WrRecord(Rec,TypeInfo) else
    JSONCustomParsers[custom].Writer(self,Rec);
end;

procedure TTextWriter.AddDynArrayJSON(const DynArray: TDynArray);
var i, n, Len, custom: integer;
    P: Pointer;
    T: TDynArrayKind;
    tmp: RawByteString;
begin // code below must match TDynArray.LoadFromJSON
  n := DynArray.Count-1;
  if n<0 then begin
    Add('[',']');
    exit;
  end;
  custom := JSONCustomParsersDynArrayIndex(DynArray.TypeInfo);
  if custom>=0 then
    T := djCustom else
    T := TypeInfoToKnown(DynArray.TypeInfo);
  P := DynArray.Value^;
  Add('[');
  case T of
  djNone: begin
    tmp := DynArray.SaveTo;
    WrBase64(pointer(tmp),length(tmp),true); // magic=true
  end;
  djCustom:
    with JSONCustomParsers[custom] do
    if Assigned(Writer) then
    for i := 0 to n do begin
      Writer(self,P^);
      Add(',');
      inc(PtrUInt(P),DynArray.ElemSize);
    end;
  djTimeLog..djSynUnicode: // add textual JSON content
    for i := 0 to n do begin
      Add('"');
      case T of
      djTimeLog:  AddTimeLog(@PInt64Array(P)^[i]);
      djDateTime: AddDateTime(@PDoubleArray(P)^[i]);
      djRawUTF8:  AddJSONEscape(PPointerArray(P)^[i]);
      djWinAnsi:
        if PPtrIntArray(P)^[i]<>0 then begin
          Len := PInteger(PPtrIntArray(P)^[i]-sizeof(Integer))^;
          if Len>length(tmp) then
            SetLength(tmp,Len+1024);
          AddJSONEscape(pointer(tmp),
            WinAnsiBufferToUtf8(pointer(tmp),PPointerArray(P)^[i],Len)-pointer(tmp));
        end;
      djString:
        if PPtrIntArray(P)^[i]<>0 then begin
          Len := PInteger(PPtrIntArray(P)^[i]-sizeof(Integer))^;
          if Len*3>length(tmp) then
            SetLength(tmp,Len*3+2048);
          AddJSONEscape(pointer(tmp),
            StringBufferToUtf8(pointer(tmp),PPointerArray(P)^[i],Len)-pointer(tmp));
        end;
      djWideString, djSynUnicode:
        if PPtrIntArray(P)^[i]<>0 then begin
          Len := PInteger(PPtrIntArray(P)^[i]-sizeof(Integer))^;
          if Len*2>length(tmp) then
            SetLength(tmp,Len*2+2048);
          AddJSONEscape(pointer(tmp),
            RawUnicodeToUtf8(pointer(tmp),length(tmp),PPointerArray(P)^[i],Len));
        end;
      end;
      Add('"',',');
    end;
  else // numerical JSON
    for i := 0 to n do begin
      case T of
      djByte:     AddU(PByteArray(P)^[i]);
      djWord:     AddU(PWordArray(P)^[i]);
      djInteger:  Add(PIntegerArray(P)^[i]);
      djCardinal: AddU(PCardinalArray(P)^[i]);
      djInt64:    Add(PInt64Array(P)^[i]);
      djDouble:   Add(PDoubleArray(P)^[i]);
      djCurrency: AddCurr64(@PInt64Array(P)^[i]);
      end;
      Add(',');
    end;
    end;
  CancelLastComma;
  Add(']');
end;

{$ifndef DELPHI5OROLDER}
procedure TTextWriter.Add(Format: PWinAnsiChar; const Values: array of const;
  Escape: TTextWriterKind=twNone);
var ValuesIndex: integer;
begin // we put const char > #127 as #??? -> asiatic MBCS codepage OK
  if Format=nil then
    exit;
  ValuesIndex := 0;
  repeat
    repeat
      case ord(Format^) of
      0: exit;
      13, 164: AddCR; // CR,¤ -> add CR,LF
      167: if B^=',' then dec(B); // §
      ord('$'),ord('%'),163,181: break; // $,%,Ј,µ
      else
        if B>=BEnd then
          FlushInc^ := Format^ else begin
          B[1] := Format^;
          inc(B);
        end;
      end;
      inc(Format);
    until false;
    // add next value as text
    if ValuesIndex<=high(Values) then // missing value will display nothing
    case ord(Format^) of
    ord('%'): with Values[ValuesIndex] do
       case Vtype of
         vtInteger:      Add(VInteger);
         vtBoolean:      AddU(byte(VBoolean));
         vtChar:         Add(@VChar,1,Escape);
         vtExtended:     Add(VExtended^);
         vtString:       Add(@VString^[1],ord(VString^[0]),Escape);
         vtPointer:      AddPointer(PtrUInt(VPointer));
         vtPChar:        Add(PUTF8Char(VPChar),Escape);
         vtObject:
           if VObject<>nil then begin
             AddShort(PShortString(PPointer(PPtrInt(VObject)^+vmtClassName)^)^);
             Add('(');
             if VObject.InheritsFrom(Exception) then
               with Exception(VObject) do
               {$ifdef UNICODE}AddW{$else}Add{$endif}(
                 pointer(Message),length(Message),Escape) else
               AddPointer(PtrUInt(VObject));
             Add(')');
           end;
         vtClass:
           if VClass<>nil then
             AddShort(PShortString(PPointer(PtrInt(VClass)+vmtClassName)^)^);
         vtWideChar:
           AddW(@VWideChar,1,Escape);
         vtPWideChar:
           AddW(pointer(VPWideChar),StrLenW(VPWideChar),Escape);
         vtAnsiString:
           Add(VAnsiString,Escape); // expect RawUTF8
         vtCurrency:
           AddCurr64(VInt64);
         vtWideString:
           if VWideString<>nil then
             AddW(VWideString,length(WideString(VWideString)),Escape);
         vtInt64:
           Add(VInt64^);
{$ifdef UNICODE}
         vtUnicodeString:
           if VUnicodeString<>nil then // convert to UTF-8
             AddW(VUnicodeString,length(UnicodeString(VUnicodeString)),Escape);
{$endif} end;
    ord('$'): with Values[ValuesIndex] do
           if Vtype=vtInteger then Add2(VInteger);
    163: with Values[ValuesIndex] do // Ј
           if Vtype=vtInteger then Add4(VInteger);
    181: with Values[ValuesIndex] do // µ
           if Vtype=vtInteger then Add3(VInteger);
    end;
    inc(Format);
    inc(ValuesIndex);
  until false;
end;
{$endif}

procedure TTextWriter.AddLine(const Text: shortstring);
begin
  if B+ord(Text[0])+2>=BEnd then
    FlushInc else
    inc(B);
  move(Text[1],B^,ord(Text[0]));
  inc(B,ord(Text[0]));
  pWord(B)^ := 13+10 shl 8; // CR + LF
  inc(B);
end;

procedure TTextWriter.AddPointer(P: PtrUInt);
{$ifndef CPU64}
var C: PUTF8Char;
{$endif}
begin
  if B+sizeof(P)*2>=BEnd then
    FlushInc else
    inc(B);
{$ifdef CPU64}
  BinToHexDisplay(@P,PAnsiChar(B),sizeof(P));
{$else}
  C := B;
  C[0] := HexChars[P shr 28];
  C[1] := HexChars[(P shr 24) and $F];
  C[2] := HexChars[(P shr 20) and $F];
  C[3] := HexChars[(P shr 16) and $F];
  C[4] := HexChars[(P shr 12) and $F];
  C[5] := HexChars[(P shr 8) and $F];
  C[6] := HexChars[(P shr 4) and $F];
  C[7] := HexChars[P and $F];
{$endif}
  inc(B,sizeof(P)*2-1);
end;

procedure TTextWriter.AddBinToHexDisplay(Bin: pointer; BinBytes: integer);
begin
  if cardinal(BinBytes*2-1)>=cardinal(fTempBufSize) then
    exit;
  if B+BinBytes*2>=BEnd then
    FlushInc else
    inc(B);
  BinToHexDisplay(Bin,PAnsiChar(B),BinBytes);
  inc(B,BinBytes*2-1);
end;

procedure TTextWriter.AddBinToHex(P: Pointer; Len: integer);
var i: integer;
begin
  if Len<=0 then
    exit;
  // inc(B) is done first to allow CancelLastChar
  if B>=BEnd then
    FlushInc
  else
    inc(B);
  repeat
    // guess biggest size to be added into buf^ at once
    i := (BEnd-B) shr 1; // div 2 -> two hexa chars per byte
    if Len<i then
      i := Len;
    // add hexa characters
    SynCommons2.BinToHex(PAnsiChar(P),PAnsiChar(B),i);
    inc(PtrInt(P),i);
    inc(B,i);
    dec(Len,i);
    if Len=0 then
      break;
    fStream.Write(fTempBuf^,B-fTempBuf); // FlushInc writes B-buf+1 -> special one here
    B := fTempBuf;
  until false;
  dec(B); // allow CancelLastChar
end;

procedure TTextWriter.AddNoJSONEscape(P: Pointer; Len: integer);
var i: integer;
begin
  if P=nil then exit;
  if Len=0 then
    Len := StrLen(PUTF8Char(P));
  if Len>0 then begin // no JSONify:
    // inc(B) is done first to allow CancelLastChar
    if B>=BEnd then
      FlushInc else
      inc(B);
    if Len=1 then begin
      // add one char
      B^ := PAnsiChar(P)^;
    end else begin
      // add two or more chars
      repeat
        // guess biggest size to be added into buf^ at once
        i := BEnd-B;
        if Len<i then
          i := Len;
        // add UTF-8 bytes
        move(P^,B^,i);
        inc(PtrInt(P),i);
        inc(B,i);
        dec(Len,i);
        if Len=0 then
          break;
        fStream.Write(fTempBuf^,B-fTempBuf); // FlushInc writes B-buf+1 -> special one here
        B := fTempBuf;
      until false;
      dec(B); // allow CancelLastChar
    end;
  end;
end;

procedure TTextWriter.AddNoJSONEscapeW(P: PWord; WideCharCount: integer);
var i: integer;
begin
  if P=nil then
    exit;
  if WideCharCount=0 then
    WideCharCount := maxInt;
  for i := 1 to WideCharCount do
  begin
    if B+5>=BEnd then
      FlushInc else
      inc(B);
    if P^=0 then
    begin
      dec(B);
      break;
    end else if P^<=126 then
      B^ := AnsiChar(ord(P^))
    else
      inc(B,UnicodeCharToUtf8(@BufUnicode[5],P^)-1);
    inc(P);
  end;
end;

procedure TTextWriter.Add(P: PUTF8Char; Escape: TTextWriterKind);
begin
  if P<>nil then
  case Escape of
    twNone:       AddNoJSONEscape(P);
    twJSONEscape: AddJSONEscape(P);
    twOnSameLine: AddOnSameLine(P);
  end;
end;

procedure TTextWriter.Add(P: PUTF8Char; Len: PtrInt; Escape: TTextWriterKind);
begin
  if P<>nil then
  case Escape of
    twNone:       AddNoJSONEscape(P,Len);
    twJSONEscape: AddJSONEscape(P,Len);
    twOnSameLine: AddOnSameLine(P,Len);
  end;
end;

procedure TTextWriter.AddW(P: PWord; Len: PtrInt; Escape: TTextWriterKind);
begin
  if P<>nil then
  case Escape of
    twNone:       AddNoJSONEscapeW(P,Len);
    twJSONEscape: AddJSONEScapeW(P,Len);
    twOnSameLine: AddOnSameLineW(P,Len);
  end;
end;

procedure TTextWriter.AddOnSameLine(P: PUTF8Char);
begin
  if P<>nil then
    while P^<>#0 do
    begin
      if B>=BEnd then
        FlushInc
      else
        inc(B);
      if P^<' ' then
        B^ := ' '
      else
        B^ := P^;
      inc(P);
    end;
end;

procedure TTextWriter.AddOnSameLine(P: PUTF8Char; Len: PtrInt);
var i: PtrInt;
begin
  if P<>nil then 
    for i := 0 to Len-1 do begin
      if B>=BEnd then
        FlushInc
      else
        inc(B);
      if P[i]<' ' then
        B^ := ' '
      else
        B^ := P[i];
    end;
end;

procedure TTextWriter.AddOnSameLineW(P: PWord; Len: PtrInt);
var i: integer;
begin
  if P=nil then exit;
  if Len=0 then
    Len := MaxInt;
  for i := 1 to Len do
  begin
    // inc(B) is done first to allow CancelLastChar
    if B>=BEnd then
      FlushInc
    else
      inc(B);
    // escape chars, so that all content will stay on the same text line
    case P^ of
      0: begin
        dec(B); break; end;
      1..32:
        B^ := ' ';
      33..126:
        B^ := AnsiChar(ord(P^)); // direct store 7 bits ASCII
      else begin // characters higher than #126 -> UTF-8 encode
        dec(B);
        AddNoJSONEscape(BufUnicode+5,
          UnicodeCharToUtf8(@BufUnicode[5],ord(P^)));
      end;
    end;
    inc(P);
  end;
end;

procedure TTextWriter.AddJSONEscape(P: Pointer; Len: PtrInt);
var c: PtrUInt;
label Esc, nxt;
begin
  if P=nil then
    exit;
  if Len=0 then
    Len := MaxInt;
  // inc(B) is done first to allow CancelLastChar
  if B>=BEnd then
    FlushInc
  else
    inc(B);
  repeat
    // escape chars, according to http://www.ietf.org/rfc/rfc4627.txt
    c := PByte(P)^;
    if c>=32 then begin
      if c in [ord('\'),ord('/'),ord('"')] then goto Esc;
      B^ := AnsiChar(c);
nxt:  if Len=1 then
        break;
      dec(Len);
      inc(PByte(P));
      if B<BEnd then begin
        inc(B);
        continue;
      end else begin
        FlushInc;
        continue;
      end;
    end else
    case c of
    0: begin
      dec(B); break; end;
    8: begin
      c := ord('b'); goto Esc; end;
    9: begin
      c := ord('t'); goto Esc; end;
    $a: begin
      c := ord('n'); goto Esc; end;
    $c: begin
      c := ord('f'); goto Esc; end;
    $d: begin
      c := ord('r');
Esc:  B^ := '\';
      if B>=BEnd then  // inlined: avoid endless loop
        FlushInc else
        inc(B);
      B^ := AnsiChar(c);
      goto nxt;
    end;
    else begin // characters below ' ', #7 e.g.
      B^ := '\';
      BufUnicode[3] := HexChars[c shr 4];
      BufUnicode[4] := HexChars[c and $F];
      AddNoJSONEscape(@BufUnicode,5); // 'u0007' e.g.
      goto nxt;
    end;
    end;
  until false;
end;

procedure TTextWriter.AddJSONEscapeW(P: PWord; Len: PtrInt);
var i: PtrInt;
    c: ansichar;
    v: PtrUInt;
label Escape;
begin
  if P=nil then exit;
  if Len=0 then
    Len := MaxInt;
  for i := 1 to Len do begin
    // inc(B) is done first to allow CancelLastChar
    if B>=BEnd then
      FlushInc else
      inc(B);
    // escape chars, according to http://www.ietf.org/rfc/rfc4627.txt
    case P^ of
      0: begin
        dec(B); break; end;
      8: begin
        c := 'b'; goto Escape; end;
      9: begin
        c := 't'; goto Escape; end;
      10: begin
        c := 'n'; goto Escape; end;
      12: begin
        c := 'f'; goto Escape; end;
      13: begin
        c := 'r'; goto Escape; end;
      ord('\'),ord('/'),ord('"'): begin // litterals 34,47,92
        c := AnsiChar(ord(P^));
Escape: B^ := '\';
        if B>=BEnd then  // inlined: avoid endless loop
          FlushInc else
          inc(B);
        B^ := c;
      end;
      32,33,35..46,48..91,93..126:
        B^ := AnsiChar(ord(P^)); // direct store 7 bits ASCII
      1..7, 11, 14..31: begin // characters below ' ', #7 e.g.
        B^ := '\';
        v := byte(PAnsiChar(P)^);
        BufUnicode[3] := HexChars[v shr 4];
        BufUnicode[4] := HexChars[v and $F];
        AddNoJSONEscape(@BufUnicode,5); // 'u0007' e.g.
      end;
      else begin // characters higher than #126 -> UTF-8 encode
        dec(B);
        AddNoJSONEscape(BufUnicode+5,
          UnicodeCharToUtf8(@BufUnicode[5],ord(P^)));
      end;
    end;
    inc(P);
  end
end;

procedure TTextWriter.AddJSONEscape(const V: TVarRec);
begin
  with V do
  case VType of
    vtString, vtAnsiString,{$ifdef UNICODE}vtUnicodeString,{$endif}
    vtPChar, vtChar, vtWideChar, vtWideString: begin
      Add('"');
      case VType of
        vtString:     AddJSONEscape(@VString^[1],ord(VString^[0]));
        vtAnsiString: AddJSONEscape(pointer(RawUTF8(VAnsiString)));
    {$ifdef UNICODE}
        vtUnicodeString: AddJSONEscapeW(pointer(string(VUnicodeString)));
    {$endif}
        vtPChar:      AddJSONEscape(VPChar);
        vtChar:       AddJSONEscape(@VChar,1);
        vtWideChar:   AddJSONEscapeW(@VWideChar,1);
        vtWideString: AddJSONEscapeW(VWideString);
      end;
      Add('"');
    end;
    vtBoolean:  AddString(JSON_BOOLEAN[VBoolean]);
    vtInteger:  Add(VInteger);
    vtInt64:    Add(VInt64^);
    vtExtended: Add(VExtended^);
    vtCurrency: AddCurr64(VInt64);
  end;
end;

procedure TTextWriter.AddJSONEscape(const NameValuePairs: array of const);
var A: integer;
begin
  Add('{');
  if high(NameValuePairs)>0 then
  for A := 0 to high(NameValuePairs) div 2 do begin
    AddJSONEscape(NameValuePairs[A*2]);
    Add(':');
    AddJSONEscape(NameValuePairs[A*2+1]);
    Add(',');
  end;
  CancelLastComma;
  Add('}');
end;

procedure TTextWriter.AddNoJSONEscapeString(const s: string);
{$ifndef UNICODE}
var tmp: PWideChar;
    L: PtrInt;
{$endif}
begin
  {$ifdef UNICODE}
  AddNoJSONEscapeW(pointer(s),length(s));
  {$else}
  L := length(S);
  GetMem(tmp,L*2+2);
  L := CurrentAnsiConvert.AnsiBufferToUnicode(tmp,pointer(s),L)-tmp;
  AddNoJSONEscapeW(pointer(tmp),L);
  Freemem(tmp);
  {$endif}
end;

procedure TTextWriter.AddJSONEscapeString(const s: string);
{$ifndef UNICODE}
var tmp: PWideChar;
    L: PtrInt;
{$endif}
begin
  {$ifdef UNICODE}
  AddJSONEscapeW(pointer(s));
  {$else}
  L := length(S);
  GetMem(tmp,L*2+2);
  L := CurrentAnsiConvert.AnsiBufferToUnicode(tmp,pointer(s),L)-tmp;
  AddJSONEscapeW(pointer(tmp),L);
  Freemem(tmp);
  {$endif}
end;

procedure TTextWriter.AddPropName(const PropName: ShortString);
begin
  if ord(PropName[0])=0 then
    exit;
  if B+ord(PropName[0])+3>=BEnd then
    FlushInc else
    inc(B);
  B^ := '"';
  move(PropName[1],B[1],ord(PropName[0]));
  inc(B,ord(PropName[0])+1);
  PWord(B)^ := ord('"')+ord(':')shl 8;
  inc(B);
end;

procedure TTextWriter.AddFieldName(const FieldName: RawUTF8);
var L: integer;
begin
  L := length(FieldName);
  if B+L+3>=BEnd then
    FlushInc else
    inc(B);
  B^ := '"';
  move(Pointer(FieldName)^,B[1],L);
  inc(B,L+1);
  PWord(B)^ := ord('"')+ord(':')shl 8;
  inc(B);
end;

procedure TTextWriter.AddClassName(aClass: TClass);
begin
  AddShort(PShortString(PPointer(PtrInt(aClass)+vmtClassName)^)^);
end;

procedure TTextWriter.AddInstanceName(Instance: TObject; SepChar: AnsiChar);
begin
  Add('"');
  if Instance=nil then
    AddShort('void') else
    AddShort(PShortString(PPointer(PPtrInt(Instance)^+vmtClassName)^)^);
  Add('(');
  AddPointer(PtrUInt(Instance));
  Add(')','"');
  if SepChar<>#0 then
    Add(SepChar);
end;

procedure TTextWriter.AddInstancePointer(Instance: TObject; SepChar: AnsiChar);
begin
  AddShort(PShortString(PPointer(PPtrInt(Instance)^+vmtClassName)^)^);
  Add('(');
  AddPointer(PtrUInt(Instance));
  Add(')');
  if SepChar<>#0 then
    Add(SepChar);
end;

procedure TTextWriter.AddShort(const Text: ShortString);
begin
  if ord(Text[0])=0 then
    exit;
  if B+ord(Text[0])>=BEnd then
    FlushInc else
    inc(B);
  move(Text[1],B^,ord(Text[0]));
  inc(B,ord(Text[0])-1);
end;

procedure TTextWriter.AddString(const Text: RawUTF8);
begin
  if PtrInt(Text)<>0 then
    AddNoJSONEscape(pointer(Text),PInteger(PtrInt(Text)-4)^);
end;

procedure TTextWriter.CancelAll;
begin
  if self=nil then
    exit; // avoid GPF
  fStream.Seek(fInitialStreamPosition,soFromBeginning);
  B := fTempBuf-1;
end;

procedure TTextWriter.CancelLastChar;
begin
  dec(B);
end;

procedure TTextWriter.CancelLastComma;
begin
  if B^=',' then
    dec(B);
end;

constructor TTextWriter.Create(aStream: TStream; aBufSize: integer);
begin
  if aStream<>nil then
  begin
    fStream := aStream;
    fInitialStreamPosition := fStream.Seek(0,soFromCurrent);
  end;
  if aBufSize<256 then
    aBufSize := 256;
  fTempBufSize := aBufSize;
  GetMem(fTempBuf,aBufSize);
  B := fTempBuf-1;
  BEnd := fTempBuf+fTempBufSize-2;
  BufUnicode := 'u0000';
end;

constructor TTextWriter.CreateOwnedStream;
begin
  Create(TRawByteStringStream.Create,4096);
  fStreamIsOwned := true;
end;

destructor TTextWriter.Destroy;
begin
  if fStreamIsOwned then
    fStream.Free;
  FreeMem(fTempBuf);
  inherited;
end;

procedure TTextWriter.Flush;
begin
  fStream.Write(fTempBuf^,B-fTempBuf+1);
  B := fTempBuf-1;
end;

function TTextWriter.FlushInc: PUTF8Char;
begin
  fStream.Write(fTempBuf^,B-fTempBuf+1);
  B := fTempBuf;
  Result := B;
end;

function TTextWriter.GetLength: integer;
begin
  if self=nil then
    result := 0
  else
    result := (B-fTempBuf+1)-fInitialStreamPosition+fStream.Seek(0,soFromCurrent);
end;

function TTextWriter.Text: RawUTF8;
begin
  Flush;
  if fStream.InheritsFrom(TRawByteStringStream) then
    if fInitialStreamPosition=0 then
      result := TRawByteStringStream(fStream).DataString else
      raise ESynException.CreateFmt(
        'TTextWriter.Text with fInitialStreamPosition=%d',[fInitialStreamPosition]) else
  if fStream.InheritsFrom(TMemoryStream) then
    SetString(result,PAnsiChar(TMemoryStream(fStream).Memory)+fInitialStreamPosition,
      TMemoryStream(fStream).Seek(0,soFromCurrent)-fInitialStreamPosition) else
    result := '';
end;

procedure TTextWriter.WrHex(P: PAnsiChar; Len: integer);
var n: integer;
begin
  Flush;
  repeat
    n := fTempBufSize shr 1;
    if Len<n then
      n := Len;
    SynCommons2.BinToHex(P,PAnsiChar(fTempBuf),n);
    fStream.Write(fTempBuf^,n*2);
    inc(P,n);
    dec(Len,n);
  until Len=0;
end;

procedure TTextWriter.WrRecord(const Rec; TypeInfo: pointer);
var L: integer;
    tmp: RawByteString;
begin
  L := RecordSaveLength(Rec,TypeInfo);
  SetString(tmp,nil,L);
  if L<>0 then
    RecordSave(Rec,pointer(tmp),TypeInfo);
  WrBase64(pointer(tmp),L,true);
end;

procedure TTextWriter.WrBase64(P: PAnsiChar; Len: cardinal; withMagic: boolean);
var trailing, main, n: cardinal;
begin
  if withMagic then
    if len<=0 then begin
      AddShort('null'); // JSON null is better than "" for BLOBs
      exit;
    end else
    AddNoJSONEscape(@JSON_BASE64_MAGIC_QUOTE_VAR,4);
  if len>0 then begin
    n := Len div 3;
    trailing := Len-n*3;
    dec(Len,trailing);
    if B+(n+1) shl 2<BEnd then begin
      // will fit in available space in Buf -> fast in-buffer Base64 encoding
      n := Base64EncodeMain(@B[1],P,Len);
      inc(B,n*4);
      inc(P,n*3);
    end else begin
      // bigger than available space in Buf -> do it per chunk
      Flush;
      while Len>0 do begin // length(buf) const -> so is ((length(buf)-4)shr2 )*3
        n := ((fTempBufSize-4)shr 2)*3;
        if Len<n then
          n := Len;
        main := Base64EncodeMain(PAnsiChar(fTempBuf),P,n);
        n := main*4;
        if n<cardinal(fTempBufSize)-4 then
          inc(B,n) else
          fStream.Write(fTempBuf^,n);
        n := main*3;
        inc(P,n);
        dec(Len,n);
      end;
    end;
    if trailing>0 then begin
      Base64EncodeTrailing(@B[1],P,trailing);
      inc(B,4);
    end;
  end;
  if withMagic then
    Add('"');
end;


{ TJSONWriter }

procedure TJSONWriter.CancelAllVoid;
const VOIDARRAY: PAnsiChar = '[]'#10;
      VOIDFIELD: PAnsiChar = '{"FieldCount":0}';
begin
  CancelAll; // rewind JSON
  if fExpand then
    fStream.Write(VOIDARRAY^,3) else
    fStream.Write(VOIDFIELD^,16); // same as sqlite3_get_table()
end;

constructor TJSONWriter.Create(aStream: TStream; Expand, withID: boolean;
  const Fields: TSQLFieldBits);
begin
  if aStream=nil then
    CreateOwnedStream else
    inherited Create(aStream);
  fExpand := Expand;
  fWithID := withID;
  fFields := Fields;
  fFieldMax := MAX_SQLFIELDS-1;
end;

procedure TJSONWriter.AddColumns(aKnownRowsCount: integer);
var i: integer;
begin
  if fExpand then begin
    for i := 0 to High(ColNames) do
      ColNames[i] := '"'+ColNames[i]+'":';
  end else begin
    AddShort('{"fieldCount":');
    Add(length(ColNames));
    if aKnownRowsCount>0 then begin
      AddShort(',"rowCount":');
      Add(aKnownRowsCount);
    end;
    AddShort(',"values":["');
    // first row is FieldNames
    for i := 0 to High(ColNames) do begin
      AddString(ColNames[i]);
      AddNoJSONEscape(PAnsiChar('","'),3);
    end;
    CancelLastChar; // cancel last '"'
    fStartDataPosition := fStream.Position+(B-fTempBuf);
     // B := buf-1 at startup -> need ',val11' position in
     // "values":["col1","col2",val11,' i.e. current pos without the ','
  end;
end;

procedure TJSONWriter.TrimFirstRow;
var P, PBegin, PEnd: PUTF8Char;
begin
  if (self=nil) or not fStream.InheritsFrom(TMemoryStream) or
     fExpand or (fStartDataPosition=0) then
    exit;
  // go to begin of first row
  Flush; // we need the data to be in fStream memory
  // PBegin^=val11 in { "fieldCount":1,"values":["col1","col2",val11,"val12",val21,..] }
  PBegin := TMemoryStream(fStream).Memory;
  PEnd := PBegin+fStream.Position;
  PEnd^ := #0; // mark end of current values
  inc(PBegin,fStartDataPosition+1); // +1 to include ',' of ',val11'
  // jump to end of first row
  P := GotoNextJSONField(PBegin,length(ColNames));
  if P=nil then exit; // unexpected end
  // trim first row data
  if P^<>#0 then
    move(P^,PBegin^,PEnd-P); // erase content
  fStream.Seek(PBegin-P,soFromCurrent); // adjust current stream position
end;


function JSONEncode(const NameValuePairs: array of const): RawUTF8;
begin
  if high(NameValuePairs)<1 then
    result := '{}' else // return void JSON object on error
    with TTextWriter.CreateOwnedStream do
    try
      AddJSONEscape(NameValuePairs);
      result := Text;
    finally
      Free
    end;
end;

function JSONEncodeArray(const Values: array of double): RawUTF8;
var W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream;
  try
    W.Add('[');
    W.AddCSV(Values);
    W.Add(']');
    result := W.Text;
  finally
    W.Free
  end;
end;

function JSONEncodeArray(const Values: array of RawUTF8): RawUTF8;
var W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream;
  try
    W.Add('[');
    W.AddCSV(Values);
    W.CancelLastComma;
    W.Add(']');
    result := W.Text;
  finally
    W.Free
  end;
end;

function JSONEncodeArray(const Values: array of integer): RawUTF8;
var W: TTextWriter;
begin
  W := TTextWriter.CreateOwnedStream;
  try
    W.Add('[');
    W.AddCSV(Values);
    W.CancelLastComma;
    W.Add(']');
    result := W.Text;
  finally
    W.Free
  end;
end;

function GetJSONFieldOrObjectOrArray(var P: PUTF8Char; wasString: PBoolean=nil;
  EndOfObject: PUTF8Char=nil; HandleValuesAsObjectOrArray: Boolean=false): PUTF8Char;
var Value: PUTF8Char;
begin
  result := nil;
  while ord(P^) in [1..32] do inc(P);
  if HandleValuesAsObjectOrArray and (P^ in ['{','[']) then begin
    Value := P;
    P := GotoNextJSONObjectOrArray(P);
    if P=nil then
      exit; // invalid content
    if wasString<>nil then
      wasString^ := false; // was object or array
    if EndOfObject<>nil then
     EndOfObject^ := P^;
    P^ := #0; // so Values[] will be a valid ASCIIZ string
    inc(P);
    result := Value;
  end else
    result := GetJSONField(P,P,wasString,EndOfObject);
end;

procedure JSONDecode(var JSON: RawUTF8;
  const Names: array of PUTF8Char; var Values: TPUtf8CharDynArray;
  HandleValuesAsObjectOrArray: Boolean=false);
begin // @JSON[1] will call UniqueString()
  JSONDecode(@JSON[1],Names,Values,HandleValuesAsObjectOrArray);
end;

function JSONDecode(P: PUTF8Char; const Names: array of PUTF8Char;
  var Values: TPUtf8CharDynArray; HandleValuesAsObjectOrArray: Boolean=false): PUTF8Char;
var n, i: PtrInt;
    Name, Value: PUTF8Char;
    EndOfObject: AnsiChar;
    NewValues, wasString: boolean;
begin
  result := nil;
  n := length(Names);
  NewValues := pointer(Values)=nil;
  SetLength(Values,n);
  if not NewValues then
    fillchar(Values[0],n*sizeof(PUTF8Char),0); // SetLength() could leave it not void
  dec(n);
  if P=nil then
    exit;
  while P^<>'{' do
    if P^=#0 then
      exit else
      inc(P);
  inc(P); // jump {
  repeat
    Name := GetJSONField(P,P,@wasString,@EndOfObject);
    if (P=nil) or (Name=nil) or not wasString or (EndOfObject<>':') then
      exit; // invalid Name:Value separator
    Value := GetJSONFieldOrObjectOrArray(P,nil,@EndOfObject,HandleValuesAsObjectOrArray);
    if (Value=nil) or not(EndOfObject in [',','}']) then
      exit; // invalid item separator
    for i := 0 to n do
      if StrIComp(Name,Names[i])=0 then begin
        Values[i] := Value;
        break;
      end;
  until (P=nil) or (EndOfObject='}');
  if P=nil then // result=nil indicates failure -> points to #0 for end of text
    result := @NULCHAR else
    result := P;
end;

function JSONDecode(var JSON: RawUTF8; const aName: RawUTF8;
  wasString: PBoolean; HandleValuesAsObjectOrArray: Boolean): RawUTF8;
var P, Name, Value: PUTF8Char;
    EndOfObject: AnsiChar;
    NameWasString: boolean;
begin
  result := '';
  P := pointer(JSON);
  if P=nil then
    exit;
  while P^<>'{' do
    if P^=#0 then
      exit else
      inc(P);
  inc(P); // jump {
  repeat
    Name := GetJSONField(P,P,@NameWasString,@EndOfObject);
    if (P=nil) or (Name=nil) or not NameWasString or (EndOfObject<>':') then
      exit; // invalid Name:Value separator
    Value := GetJSONFieldOrObjectOrArray(
      P,wasString,@EndOfObject,HandleValuesAsObjectOrArray);
    if (Value=nil) or not(EndOfObject in [',','}']) then
      exit; // invalid item separator
    if StrIComp(Name,pointer(aName))=0 then begin
      Result := RawUTF8(Value);
      exit;
    end;
  until (P=nil) or (EndOfObject='}');
end;

const
  EndOfJSONValueField: TSetOfAnsiChar = [#0,#9,#10,#13,' ',',','}',']'];
  EndOfJSONField: TSetOfAnsiChar = [',',']','}',':'];
  DigitChars: TSetOfAnsiChar = ['-','+','0'..'9'];
  DigitFirstChars: TSetOfAnsiChar = ['-','1'..'9']; // 0/- excluded by JSON! 
  DigitFloatChars: TSetOfAnsiChar = ['-','+','0'..'9','.'];
  NULL_DF  = ord('N')+ord('U')shl 8+ord('L')shl 16+ord('L')shl 24;
  FALSE_DF = ord('F')+ord('A')shl 8+ord('L')shl 16+ord('S')shl 24;
  TRUE_DF  = ord('T')+ord('R')shl 8+ord('U')shl 16+ord('E')shl 24;

/// decode a JSON field into an UTF-8 encoded buffer, stored inplace of JSON data
function GetJSONField(P: PUTF8Char; out PDest: PUTF8Char;
  wasString: PBoolean=nil; EndOfObject: PUTF8Char=nil): PUTF8Char;
// this code is very fast
var U: PUTF8Char;
    w: word;
    c4: integer;
begin
  if wasString<>nil then
    wasString^ := false; // default is 'no string'
  PDest := nil; // mark error or unexpected end (#0)
  result := nil;
  if P=nil then exit;
  if P^<=' ' then repeat inc(P); if P^=#0 then exit; until P^>' ';
  c4 := NormToUpperAnsi7Byte[ord(P[0])]+NormToUpperAnsi7Byte[ord(P[1])] shl 8+
    NormToUpperAnsi7Byte[ord(P[2])] shl 16+NormToUpperAnsi7Byte[ord(P[3])] shl 24;
  //c4 := PInteger(P)^ and $DFDFDFDF;
  if (c4=NULL_DF) and (P[4] in EndOfJSONValueField)  then begin
    result := nil; // null value (could occur for "") -> return nil = ''
    inc(P,3);
  end else
  if (c4=FALSE_DF) and (NormToUpperAnsi7Byte[ord(P[4])]=ord('E')) and
     (P[5] in EndOfJSONValueField) then begin
    PWord(P)^ := ord('0');
    result := P; // false -> return '0'
    inc(P,4);
  end else
  if (c4=TRUE_DF) and (P[4] in EndOfJSONValueField)  then begin
    PWord(P)^ := ord('1');
    result := P; // true -> return '1'
    inc(P,3);
  end else
  if P^='"' then begin
    // '"string \"\\field"' -> 'string "\field'
    if wasString<>nil then
      wasString^ := true;
    inc(P);
    U := P;
    result := P;
    repeat // unescape P^ into U^ (cf. http://www.ietf.org/rfc/rfc4627.txt)
      case P^ of
      #0:  exit;  // leave PDest=nil for unexpected end
      '"': break; // end of string
      '\': begin
        inc(P);
        case P^ of // unescape JSON string
          'b': U^ := #08;
          't': U^ := #09;
          'n': U^ := #$0a;
          'f': U^ := #$0c;
          'r': U^ := #$0d;
          'u': 
          if HexToBin(pointer(P+1),@w,2) then begin // '\u0123'
            w := swap(w);  // reverse byte order
            if w<=$7f then // speed up UTF-8 encoding for most used chars
              U^ := ansichar(w) else
              inc(U,UnicodeCharToUtf8(U,w)-1);
            inc(P,4);
          end else
            U^ := '?'; // bad formated hexa number -> '?0123'
          else U^ := P^; // litterals: '\"' -> '"'
        end;
      end;
      else U^ := P^;
      end; // case P^ of
      inc(U);
      inc(P);
    until false;
    U^ := #0; // make zero-terminated
  end else begin
    // numerical field: all chars before end of field
    if not (P[0] in DigitFirstChars) then // is first char (at least) a number?
      if (P[0]<>'0') or (P[1] in ['0'..'9']) then // 0123 excluded by JSON!
        exit; // leave PDest=nil for unexpected end
    result := P;
    repeat
      inc(P);
    until not (P^ in DigitFloatChars);
    if P^=#0 then
      exit;
    if P^<=' ' then
      P^ := #0; // force numerical field with no trailing ' '
  end;
  while not (P^ in EndOfJSONField) do begin
    inc(P);
    if P^=#0 then
      exit; // leave PDest=nil for unexpected end
  end;
  if EndOfObject<>nil then
    EndOfObject^ := P^;
  P^ := #0; // make zero-terminated
  PDest := @P[1];
  if P[1]=#0 then
    PDest := nil;
end;

function IsString(P: PUTF8Char): boolean;  // test if P^ is a "string" value
begin
  if P=nil then begin
    result := false;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  if (P[0] in ['0'..'9']) or // is first char numeric?
     ((P[0] in ['-','+']) and (P[1] in ['0'..'9'])) then begin
    // check if P^ is a true numerical value
    repeat inc(P) until not (P^ in ['0'..'9']); // check digits
    if P^='.' then
      repeat inc(P) until not (P^ in ['0'..'9']); // check fractional digits
    if (P^ in ['e','E']) and (P[1] in DigitChars) then begin
      inc(P);
      if P^='+' then inc(P) else
      if P^='-' then inc(P);
      while P^ in ['0'..'9'] do inc(P);
    end;
    if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
    result := (P^<>#0);
    exit;
  end else
    result := true; // don't begin with a numerical value -> must be a string
end;

function IsStringJSON(P: PUTF8Char): boolean;  // test if P^ is a "string" value
var c4: integer;
begin
  if P=nil then begin
    result := false;
    exit;
  end;
  if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
  c4 := NormToUpperAnsi7Byte[ord(P[0])]+NormToUpperAnsi7Byte[ord(P[1])] shl 8+
    NormToUpperAnsi7Byte[ord(P[2])] shl 16+NormToUpperAnsi7Byte[ord(P[3])] shl 24;
  if ((c4=NULL_DF) and (P[4] in EndOfJSONValueField)) or
     ((c4=FALSE_DF) and (NormToUpperAnsi7Byte[ord(P[4])]=ord('E')) and
      (P[5] in EndOfJSONValueField)) or
     ((c4=TRUE_DF) and (P[4] in EndOfJSONValueField)) then begin
    result := false; // constants are no string
    exit;
  end else
  if (P[0] in ['1'..'9']) or // is first char numeric?
     ((P[0]='0') and not (P[1] in ['0'..'9'])) or // '012' excluded by JSON
     ((P[0]='-') and (P[1] in ['0'..'9'])) then begin
    // check if P^ is a true numerical value
    repeat inc(P) until not (P^ in ['0'..'9']); // check digits
    if P^='.' then
      repeat inc(P) until not (P^ in ['0'..'9']); // check fractional digits
    if (P^ in ['e','E']) and (P[1] in DigitChars) then begin
      inc(P);
      if P^='+' then inc(P) else
      if P^='-' then inc(P);
      while P^ in ['0'..'9'] do inc(P);
    end;
    if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
    result := (P^<>#0);
    exit;
  end else
    result := true; // don't begin with a numerical value -> must be a string
end;

function GotoNextJSONField(P: PUTF8Char; FieldCount: cardinal): PUTF8Char;
var F: integer;
begin
 result := nil; // to notify unexpected end
 for F := 1 to FieldCount do begin
   // get a field
   if P^='"' then  // "string \"field"
     repeat
       inc(P);
       if P^=#0 then exit else // unexpected end
       if P^='"' then
         if P[-1]<>'\' then  // 'sdklds\"' is invalid ending
           break else
           if P[-2]='\' then break; //   '\\"' is valid ending
     until false;
   repeat // end of field: ,]
     inc(P);
     if P^=#0 then exit; // unexpected end
   until P^ in [',',']'];
   repeat inc(P) until P^<>' '; // jmp ,] and next spaces
 end;
 result := P;
end;

function GotoNextJSONObjectOrArray(P: PUTF8Char): PUTF8Char;
var EndChar: AnsiChar;
begin
  result := nil; // mark error or unexpected end (#0)
  while P^ in [#1..' '] do inc(P);
  case P^ of
  '[': EndChar := ']';
  '{': EndChar := '}';
  else exit;
  end;
  repeat inc(P) until not(P^ in [#1..' ']);
  repeat
    case P^ of
    '{','[': begin
      P := GotoNextJSONObjectOrArray(P);
      if P=nil then exit;
    end;
    ':': if EndChar<>'}' then exit else inc(P); // syntax for JSON object only
    ',': inc(P); // comma appears in both JSON objects and arrays
    '}': if EndChar='}' then break else exit;
    ']': if EndChar=']' then break else exit;
    '"': begin
      repeat
        inc(P);
        if P^=#0 then exit else
        if P^='"' then
          if P[-1]<>'\' then  // 'sdklds\"' is invalid ending
            break else
            if P[-2]='\' then break; //   '\\"' is valid ending
      until false;
      inc(P);
    end;
    '-','+','0'..'9': // '0123' excluded by JSON, but not handled here
      repeat
        inc(P);
      until not (P^ in ['0'..'9','.','E','e']);
    't','T': if IdemPChar(P+1,'RUE') then inc(P,4) else exit;
    'f','F': if IdemPChar(P+1,'ALSE') then inc(P,5) else exit;
    else Exit;
    end;
    while P^ in [#1..' '] do inc(P);
  until P^=EndChar;
  result := P+1;
end;

{ ************ filtering and validation classes and functions }

function IsValidIP4Address(P: PUTF8Char): boolean;
var ndot: PtrInt;
    V: PtrUInt;
begin
  result := false;
  if (P=nil) or not (P^ in ['0'..'9']) then
    exit;
  V := 0;
  ndot := 0;
  repeat
    case P^ of
      #0: break;
      '.': if (P[-1]='.') or (V>255) then
        exit else begin
        inc(ndot);
        V := 0;
      end;
      '0'..'9': V := (V*10)+ord(P^)-48;
      else exit;
    end;
    inc(P);
  until false;
  if (ndot=3) and (V<=255) and (P[-1]<>'.') then
    result := true;
end;

function IsValidEmail(P: PUTF8Char): boolean;
// Initial Author: Ernesto D'Spirito - UTF-8 version by AB
// http://www.howtodothings.com/computers/a1169-validating-email-addresses-in-delphi.html
const
  // Valid characters in an "atom"
  atom_chars: set of AnsiChar = [#33..#255] -
     ['(', ')', '<', '>', '@', ',', ';', ':', '\', '/', '"', '.', '[', ']', #127];
  // Valid characters in a "quoted-string"
  quoted_string_chars: set of AnsiChar = [#0..#255] - ['"', #13, '\'];
  // Valid characters in a subdomain
  letters: set of AnsiChar = ['A'..'Z', 'a'..'z'];
  letters_digits: set of AnsiChar = ['0'..'9', 'A'..'Z', 'a'..'z'];
type
  States = (STATE_BEGIN, STATE_ATOM, STATE_QTEXT, STATE_QCHAR,
    STATE_QUOTE, STATE_LOCAL_PERIOD, STATE_EXPECTING_SUBDOMAIN,
    STATE_SUBDOMAIN, STATE_HYPHEN);
var
  State: States;
  subdomains: integer;
  c: AnsiChar;
  ch: PtrInt;
begin
  State := STATE_BEGIN;
  subdomains := 1;
  if P<>nil then
  repeat
    ch := NextUTF8Char(P,P);
    if (ch<=255) and (WinAnsiConvert.AnsiToWide[ch]<=255) then
      // convert into WinAnsi char
      c := AnsiChar(ch) else
      // invalid char
      c := #127;
    case State of
    STATE_BEGIN:
      if c in atom_chars then
        State := STATE_ATOM else
      if c = '"' then
        State := STATE_QTEXT else
        break;
    STATE_ATOM:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c = '.' then
        State := STATE_LOCAL_PERIOD else
      if not (c in atom_chars) then
        break;
    STATE_QTEXT:
      if c = '\' then
        State := STATE_QCHAR else
      if c = '"' then
        State := STATE_QUOTE else
      if not (c in quoted_string_chars) then
        break;
    STATE_QCHAR:
      State := STATE_QTEXT;
    STATE_QUOTE:
      if c = '@' then
        State := STATE_EXPECTING_SUBDOMAIN else
      if c = '.' then
        State := STATE_LOCAL_PERIOD else
        break;
    STATE_LOCAL_PERIOD:
      if c in atom_chars then
        State := STATE_ATOM else
      if c = '"' then
        State := STATE_QTEXT else
        break;
    STATE_EXPECTING_SUBDOMAIN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
        break;
    STATE_SUBDOMAIN:
      if c = '.' then begin
        inc(subdomains);
        State := STATE_EXPECTING_SUBDOMAIN
      end else
      if c = '-' then
        State := STATE_HYPHEN else
      if not (c in letters_digits) then
        break;
    STATE_HYPHEN:
      if c in letters_digits then
        State := STATE_SUBDOMAIN else
      if c <> '-' then
        break;
    end;
    if P^=#0 then begin
      P := nil;
      break;
    end;
  until false;
  Result := (State = STATE_SUBDOMAIN) and (subdomains >= 2);
end;


function IsMatch(const Pattern, Text: RawUTF8; CaseInsensitive: boolean): boolean;
// code below adapted from ZMatchPattern.pas - http://www.zeoslib.sourceforge.net

  type
    TMatch = (mNONE, mABORT, mEND, mLITERAL, mPATTERN, mRANGE, mVALID);
  const
    SINGLE	= '?';
    KLEENE_STAR = '*';
    RANGE_OPEN	= '[';
    RANGE = '-';
    RANGE_CLOSE = ']';
    CARET_NEGATE = '^';
    EXCLAMATION_NEGATE	= '!';

  function MatchAfterStar(Pattern, Text: RawUTF8): TMatch; forward;

  function Matche(const Pattern, Text: RawUTF8): TMatch;
  var RangeStart, RangeEnd, P, T, PLen, TLen: Integer;
      Invert, MemberMatch: Boolean;
  begin
    P := 1;
    T := 1;
    PLen := Length(pattern);
    TLen := Length(text);
    Result := mNONE;
    while ((Result = mNONE) and (P <= PLen)) do begin
      if T > TLen then begin
        if (Pattern[P] = KLEENE_STAR) and (P+1 > PLen) then
          Result := mVALID else
          Result := mABORT;
        exit;
      end else
      case Pattern[P] of
        KLEENE_STAR:
          Result := MatchAfterStar(Copy(Pattern,P,PLen),Copy(Text,T,TLen));
        RANGE_OPEN: begin
          Inc(P);
          Invert := False;
          if (Pattern[P] = EXCLAMATION_NEGATE) or
            (Pattern[P] = CARET_NEGATE) then begin
            Invert := True;
            Inc(P);
          end;
          if (Pattern[P] = RANGE_CLOSE) then begin
            Result := mPATTERN;
            Exit;
          end;
          MemberMatch := False;
          while Pattern[P] <> RANGE_CLOSE do begin
            RangeStart := P;
            RangeEnd := P;
            Inc(P);
            if P > PLen then begin
              Result := mPATTERN;
              Exit;
            end;
            if Pattern[P] = RANGE then begin
              Inc(P);
              RangeEnd := P;
              if (P > PLen) or (Pattern[RangeEnd] = RANGE_CLOSE) then begin
                Result := mPATTERN;
                Exit;
              end;
              Inc(P);
            end;
            if P > PLen then begin
              Result := mPATTERN;
              Exit;
            end;
            if RangeStart < RangeEnd then begin
              if (Text[T] >= Pattern[RangeStart]) and
                 (Text[T] <= Pattern[RangeEnd]) then begin
                MemberMatch := True;
                break;
              end;
            end
            else begin
              if (Text[T] >= Pattern[RangeEnd]) and
                 (Text[T] <= Pattern[RangeStart]) then begin
                MemberMatch := True;
                break;
              end;
            end;
          end;
          if (Invert and MemberMatch) or (not (Invert or MemberMatch)) then begin
            Result := mRANGE;
            Exit;
          end;
          if MemberMatch then
            while (P <= PLen) and (Pattern[P] <> RANGE_CLOSE) do
              Inc(P);
          if P > PLen then begin
            Result := mPATTERN;
            Exit;
          end;
        end;
      else
        if Pattern[P] <> SINGLE then
          if Pattern[P] <> Text[T] then
            Result := mLITERAL;
      end;
      Inc(P);
      Inc(T);
    end;
    if Result = mNONE then
      if T <= TLen then
        Result := mEND else
        Result := mVALID;
  end;

  function MatchAfterStar(Pattern, Text: RawUTF8): TMatch;
  var P, T, PLen, TLen: Integer;
  begin
    Result := mNONE;
    P := 1;
    T := 1;
    PLen := Length(Pattern);
    TLen := Length(Text);
    if TLen = 1 then begin
      Result := mVALID;
      Exit;
    end else
    if (PLen = 0) or (TLen = 0) then begin
      Result := mABORT;
      Exit;
    end;
    while ((T <= TLen) and (P < PLen)) and ((Pattern[P] = SINGLE) or
      (Pattern[P] = KLEENE_STAR)) do begin
      if Pattern[P] = SINGLE then
        Inc(T);
      Inc(P);
    end;
    if T >= TLen then begin
      Result := mABORT;
      Exit;
    end else
    if P >= PLen then begin
      Result := mVALID;
      Exit;
    end;
    repeat
      if (Pattern[P] = Text[T]) or (Pattern[P] = RANGE_OPEN) then begin
        Pattern := Copy(Pattern, P, PLen);
        Text := Copy(Text, T, TLen);
        PLen := Length(Pattern);
        TLen := Length(Text);
        p := 1;
        t := 1;
        Result  := Matche(Pattern, Text);
        if Result <> mVALID then
          Result := mNONE; // retry until end of Text, (check below) or Result valid
      end;
      Inc(T);
      if (T > TLen) or (P > PLen) then begin
        Result := mABORT;
        Exit;
      end;
    until Result <> mNONE;
  end;

begin // IsMatch() main block
  if CaseInsensitive then
    Result := (Matche(LowerCase(Pattern), LowerCase(Text)) = mVALID) else
    Result := (Matche(Pattern, Text) = mVALID);
end;


{ TSynFilterOrValidate }

constructor TSynFilterOrValidate.Create(const aParameters: RawUTF8);
begin
  inherited Create;
  SetParameters(aParameters); // should parse the JSON-encoded parameters
end;

procedure TSynFilterOrValidate.SetParameters(Value: RawUTF8);
begin
  fParameters := Value;
end;


{ TSynFilterUpperCase }

procedure TSynFilterUpperCase.Process(aFieldIndex: integer; var Value: RawUTF8);
begin
  Value := UpperCase(Value);
end;


{ TSynFilterUpperCaseU }

procedure TSynFilterUpperCaseU.Process(aFieldIndex: integer; var Value: RawUTF8);
begin
  Value := UpperCaseU(Value);
end;


{ TSynFilterLowerCase }

procedure TSynFilterLowerCase.Process(aFieldIndex: integer; var Value: RawUTF8);
begin
  Value := LowerCase(Value);
end;


{ TSynFilterLowerCaseU }

procedure TSynFilterLowerCaseU.Process(aFieldIndex: integer; var Value: RawUTF8);
begin
  Value := LowerCaseU(Value);
end;


{ TSynFilterTrim }

procedure TSynFilterTrim.Process(aFieldIndex: integer; var Value: RawUTF8);
begin
  Value := trim(Value);
end;


{ TSynValidateIPAddress }

function TSynValidateIPAddress.Process(aFieldIndex: integer; const Value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  result := IsValidIP4Address(pointer(Value));
  if not result then
    ErrorMsg := Format(sInvalidIPAddress,[UTF8ToString(Value)]);
end;


{ TSynValidateEmail }

function TSynValidateEmail.Process(aFieldIndex: integer; const Value: RawUTF8;
  var ErrorMsg: string): boolean;
var TLD: RawUTF8;
    i: integer;
const TopLevelTLD: array[0..19] of PUTF8Char = (
  // see http://en.wikipedia.org/wiki/List_of_Internet_top-level_domains
  'aero','asia','biz','cat','com','coop','edu','gov','info','int','jobs',
  'mil','mobi','museum','name','net','org','pro','tel','travel'); // no xxx !
begin
  result := IsValidEmail(pointer(Value));
  if result then begin
    i := length(Value);
    while (i>0) and (Value[i]<>'.') do dec(i);
    TLD := lowercase(copy(Value,i+1,100));
    if FastFindPUTF8CharSorted(@TopLevelTLD,high(TopLevelTLD),pointer(TLD),StrComp)<0 then
      if length(TLD)<>2 then
        // if not a Top-Level TLD, guess a valid country is a two chars string
        result := false;
    if result and (AllowedTLD<>'') then
      result := FindCSVIndex(pointer(AllowedTLD),TLD)>=0;
    if result and (ForbiddenTLD<>'') then
      result := FindCSVIndex(pointer(ForbiddenTLD),TLD)<0;
    if result and (ForbiddenDomains<>'') then begin
      TLD := lowercase(copy(Value,PosEx('@',Value)+1,100));
      result := FindCSVIndex(pointer(ForbiddenDomains),TLD)<0;
    end;
  end;
  if not result then
    ErrorMsg := Format(sInvalidEmailAddress,[UTF8ToString(Value)]);
end;

procedure TSynValidateEmail.SetParameters(Value: RawUTF8);
var V: TPUtf8CharDynArray;
begin
  inherited;
  JSONDecode(Value,['AllowedTLD','ForbiddenTLD','ForbiddenDomains'],V);
  AllowedTLD := LowerCase(V[0]);
  ForbiddenTLD := LowerCase(V[1]);
  ForbiddenDomains  := LowerCase(V[2]);
end;


{ TSynValidatePattern }

function TSynValidatePattern.Process(aFieldIndex: integer; const Value: RawUTF8;
  var ErrorMsg: string): boolean;
begin
  result := IsMatch(fParameters,Value,ClassType=TSynValidatePatternI);
  if not result then
    ErrorMsg := Format(sInvalidPattern,[UTF8ToString(Value)]);
end;


{ TSynValidateText }

function Character01n(n: integer): string;
begin
  if n<0 then
    n := 0 else
  if n>1 then
    n := 2;
  result := GetCSVItemString(pointer(string(sCharacter01n)),n);
end;

constructor TSynValidateText.Create(const aParameters: RawUTF8);
begin
  MinLength := 1;
  MaxLength := maxInt;
  MaxSpaceCount := maxInt;
  MaxLeftTrimCount := maxInt;
  MaxRightTrimCount := maxInt;
  inherited;
end;

function TSynValidateText.ErrorMsg(fPropsIndex, InvalidTextIndex,
  MainIndex: integer): string;
var P: PChar;
begin
  P := pointer(string(sInvalidTextChar));
  result := GetCSVItemString(P,MainIndex);
  if fPropsIndex>0 then
    result := Format(result,
      [fProps[fPropsIndex],GetCSVItemString(P,InvalidTextIndex),
       Character01n(fProps[fPropsIndex])]);
end;

function TSynValidateText.Process(aFieldIndex: integer; const Value: RawUTF8;
  var ErrorMsg: string): boolean;
var i, L: cardinal;
    Min: array[2..7] of cardinal;
begin
  result := false;
  L := Utf8ToUnicodeLength(pointer(Value));
  if L<MinLength then
    ErrorMsg := Format(sInvalidTextLengthMin,[MinLength,Character01n(MinLength)]) else
  if L>MaxLength then
    ErrorMsg := Format(sInvalidTextLengthMax,[MaxLength,Character01n(MaxLength)]) else begin
    fillchar(Min,sizeof(Min),0);
    L := length(Value);
    for i := 1 to L do
    case Value[i] of
      ' ':
        inc(Min[7]);
      'a'..'z': begin
        inc(Min[2]);
        inc(Min[5]);
      end;
      'A'..'Z': begin
        inc(Min[2]);
        inc(Min[6]);
      end;
      '0'..'9':
        inc(Min[3]);
      '_','!',';','.',',','/',':','?','%','$','=','"','#','@','(',')','{','}',
      '+','''','-','*':
        inc(Min[4]);
    end;
    for i := 2 to 7 do
      if Min[i]<fProps[i] then begin
        ErrorMsg := self.ErrorMsg(i,i,0);
        exit;
      end;
    if Min[7]>MaxSpaceCount then begin
      ErrorMsg := self.ErrorMsg(8,7,1);
      exit;
    end;
    if Value<>'' then begin
      if MaxLeftTrimCount<cardinal(maxInt) then begin
        // if MaxLeftTrimCount is set, check against Value
        i := 0;
        while (i<L) and (Value[i+1]=' ') do inc(i);
        if i>MaxLeftTrimCount then begin
          ErrorMsg := self.ErrorMsg(0,0,8);
          exit;
        end;
      end;
      if MaxRightTrimCount<cardinal(maxInt) then begin
        // if MaxRightTrimCount is set, check against Value
        i := 0;
        while (i<L) and (Value[L-i]=' ') do dec(i);
        if i>MaxRightTrimCount then begin
          ErrorMsg := self.ErrorMsg(0,0,9);
          exit;
        end;
      end;
    end;
    result := true;
  end;
end;

procedure TSynValidateText.SetParameters(Value: RawUTF8);
var V: TPUtf8CharDynArray;
    i: integer;
begin
  inherited;
  JSONDecode(Value,['MinLength','MaxLength',
    'MinAlphaCount','MinDigitCount','MinPunctCount',
    'MinLowerCount','MinUpperCount','MaxSpaceCount','MaxSpaceCount',
    'MaxLeftTrimCount','MaxRightTrimCount'],V);
  for i := 0 to high(fProps) do
    fProps[i] := GetCardinalDef(V[i],fProps[i]);
end;


{ TSynValidatePassWord }

procedure TSynValidatePassWord.SetParameters(Value: RawUTF8);
begin
  // set default values for validating a strong password
  MinLength := 5;
  MaxLength := 10;
  MinAlphaCount := 1;
  MinDigitCount := 1;
  MinPunctCount := 1;
  MinLowerCount := 1;
  MinUpperCount := 1;
  MaxSpaceCount := 0;
  // read custom parameters
  inherited;
end;


{ ************ Unit-Testing classes and functions }

function KB(bytes: Int64): RawUTF8;
begin
  if bytes>=1024*1024 then begin
    if bytes>=1024*1024*1024 then begin
      bytes := bytes shr 20;
      result := ' GB';
    end else
      result := ' MB';
    result :=
      UInt32ToUtf8(bytes shr 20)+'.'+
      UInt32ToUtf8((PtrUInt(bytes) and pred(1 shl 20))div (102*1024))+
      result;
  end else
  if bytes>1023*9 then
    result := UInt32ToUtf8(PtrUInt(bytes) shr 10)+' KB' else
    result := UInt32ToUtf8(PtrUInt(bytes))+' B';
end;

function IntToThousandString(Value: integer; const ThousandSep: RawUTF8=','): RawUTF8;
var i,L,Len: cardinal;
begin
  Result := Int32ToUtf8(value);
  L := length(Result);
  Len := L+1;
  if Value<0 then
    dec(L,2) else // ignore '-' sign
    dec(L);
  for i := 1 to L div 3 do
    insert(ThousandSep,Result,Len-i*3);
end;

function MicroSecToString(Micro: Int64): RawUTF8;
function TwoDigitToString(value: cardinal): RawUTF8;
var L: integer;
begin
  result := UInt32ToUtf8(value);
  L := length(result);
  if L=1 then
    result := '0.0'+result else // '3' -> '0.03'
  if L=2 then
    result := '0.'+result else // '35' -> '0.35'
    insert('.',result,L-1); // '103' -> '1.03'
end;
begin
  if Micro<=0 then
    result := '0us' else
  if Micro<1000 then
    result := UInt32ToUtf8(Int64Rec(Micro).Lo)+'us' else
  if Micro<1000*1000 then
    result := TwoDigitToString(Micro div 10)+'ms' else
    result := TwoDigitToString(Micro div (10*1000))+'s';
end;

{ TSynCache }

procedure TSynCache.Add(const aValue: RawUTF8; aTag: PtrInt);
begin
  if (self=nil) or (fFindLastAddedIndex<0) then
    exit;
  inc(fValueSize,length(aValue));
  if fValueSize>fMaxCacheRamUsed then
    // if tends to consume too much memory, restart caching (fast in practice)
    Reset else
    // add the cache entry values (text+integer)
    with fNameValue.List[fFindLastAddedIndex] do begin
      Tag := aTag;
      Value := aValue;
    end;
  fFindLastAddedIndex := -1;
end;

constructor TSynCache.Create(aMaxCacheRamUsed: cardinal=16384*1024);
begin
  fNameValue.Init(false);
  fNameValue.fDynArray.Capacity := 200; // some space for future cached entries
  fMaxCacheRamUsed := aMaxCacheRamUsed;
  fFindLastAddedIndex := -1;
end;

function TSynCache.Find(const aKey: RawUTF8; aResultTag: PPtrInt): RawUTF8;
var added: boolean;
begin
  if (self=nil) or (aKey='') then begin
    fFindLastAddedIndex := -1;
    result := '';
  end else begin
    fFindLastAddedIndex := fNameValue.fDynArray.FindHashedForAdding(aKey,added);
    if added then begin
      fNameValue.List[fFindLastAddedIndex].Name := aKey;
      result := '';
    end else
    // match key found
    with fNameValue.List[fFindLastAddedIndex] do begin
      result := Value;
      if aResultTag<>nil then
        aResultTag^ := Tag;
      fFindLastAddedIndex := -1;
    end;
  end;
end;

function TSynCache.Reset: boolean;
begin
  result := false;
  if self=nil then
    exit; // avoid GPF
  if Count<>0 then begin
    if Count<10 then // no need to change capacity for small cache content
      fNameValue.Count := 0 else
      with fNameValue.fDynArray do begin
        Capacity := 0;   // force free all fNameValue.List[] key/value pairs
        Capacity := 200; // then reserve some space for future cached entries
      end;
    fNameValue.fDynArray.ReHash; // will force reset all hash content
    result := true; // mark something was flushed
  end;
  fFindLastAddedIndex := -1;
  fValueSize := 0;
end;


{ TRawUTF8List }

function TRawUTF8List.Add(const aText: RawUTF8): PtrInt;
begin
  result := AddObject(aText,nil);
end;

function TRawUTF8List.AddObject(const aText: RawUTF8; aObject: TObject): PtrInt;
var n: PtrInt;
begin
  if self=nil then begin
    result := -1;
    exit;
  end;
  n := length(fList);
  result := fCount;
  if result>=n then begin
    inc(n,256);
    SetLength(fList,n);
    if (fObjects<>nil) or (aObject<>nil) then
      SetLength(fObjects,n);
  end else
    if (aObject<>nil) and (fObjects=nil) then
      SetLength(fObjects,n);
  fList[result] := aText;
  if aObject<>nil then
    fObjects[result] := aObject;
  inc(fCount);
  Changed;
end;

procedure TRawUTF8List.AddRawUTF8List(List: TRawUTF8List);
var i: PtrInt;
begin
  if List<>nil then begin
    BeginUpdate;
    for i := 0 to List.fCount-1 do
      AddObject(List.fList[i],List.fObjects[i]);
    EndUpdate;
  end;
end;

procedure TRawUTF8List.BeginUpdate;
begin
  inc(fOnChangeLevel);
  if fOnChangeLevel>0 then
    exit;
  fOnChangeHidden := fOnChange;
  fOnChange := OnChangeHidden;
  fOnChangeTrigerred := false;
end;

procedure TRawUTF8List.Changed;
begin
  if (self<>nil) and Assigned(fOnChange) then
    fOnChange(self);
end;

procedure TRawUTF8List.Clear;
begin
  Capacity := 0;
  Changed;
end;

constructor TRawUTF8List.Create;
begin
  fNameValueSep := '=';
end;

procedure TRawUTF8List.Delete(Index: PtrInt);
begin
  if (self=nil) or (PtrUInt(Index)>=PtrUInt(fCount)) then
    exit;
  fList[Index] := ''; // release corresponding memory
  dec(fCount);
  if Index<fCount then begin
    move(fList[Index+1],fList[Index],(fCount-Index)*sizeof(fList[0]));
    if fObjects<>nil then
      move(fObjects[Index+1],fObjects[Index],(fCount-Index)*sizeof(fObjects[0]));
  end;
  Changed;
end;

function TRawUTF8List.DeleteFromName(const Name: RawUTF8): PtrInt;
begin
  Result := IndexOfName(Name);
  if Result>=0 then
    Delete(Result);
end;

procedure TRawUTF8List.EndUpdate;
begin
  if fOnChangeLevel<=0 then
    exit;
  dec(fOnChangeLevel);
  if fOnChangeLevel>0 then
    exit; // allows nested BeginUpdate..EndUpdate calls
  fOnChange := fOnChangeHidden;
  if fOnChangeTrigerred and Assigned(fOnChange) then
    fOnChange(self);
  fOnChangeTrigerred := false;
end;

function TRawUTF8List.Get(Index: PtrInt): RawUTF8;
begin
  if (self=nil) or (PtrUInt(Index)>=PtrUInt(fCount)) then
    result := '' else
    result := fList[Index];
end;

function TRawUTF8List.GetCapacity: PtrInt;
begin
  if self=nil then
    result := 0 else
    result := length(fList);
end;

function TRawUTF8List.GetCount: PtrInt;
begin
  if self=nil then
    result := 0 else
    result := fCount;
end;

function TRawUTF8List.GetListPtr: PPUtf8CharArray;
begin
  if self=nil then
    result := nil else
    result := pointer(fList);
end;

function TRawUTF8List.GetObjectPtr: PPointerArray;
begin
  if self=nil then
    result := nil else
    result := pointer(fObjects);
end;

function TRawUTF8List.GetName(Index: PtrInt): RawUTF8;
var Sep: PUTF8Char;
begin
  result := Get(Index);
  if result='' then
    exit;
  Sep := PosChar(pointer(result),NameValueSep);
  if Sep=nil then
    result := '' else
    SetLength(result,Sep-pointer(result));
end;

function TRawUTF8List.GetObject(Index: PtrInt): TObject;
begin
  if (self<>nil) and (PtrUInt(Index)<PtrUInt(fCount)) and (fObjects<>nil) then
    result := fObjects[Index] else
    result := nil;
end;

function TRawUTF8List.GetText(const Delimiter: RawUTF8): RawUTF8;
var DelimLen, i, Len: PtrInt;
    P: PUTF8Char;
begin
  result := '';
  if (self=nil) or (fCount=0) then
    exit;
  DelimLen := length(Delimiter);
  Len := DelimLen*fCount;
  for i := 0 to fCount-1 do
    inc(Len,length(fList[i]));
  SetLength(result,len);
  P := pointer(result);
  i := 0;
  repeat
    Len := length(fList[i]);
    if Len>0 then begin
      Move(pointer(fList[i])^,P^,Len);
      inc(P,Len);
    end;
    inc(i);
    if i>=fCount then
      Break;
    Move(pointer(Delimiter)^,P^,DelimLen);
    inc(P,DelimLen);
  until false;
end;

function TRawUTF8List.GetTextCRLF: RawUTF8;
begin
  result := GetText;
end;

function TRawUTF8List.GetValue(const Name: RawUTF8): RawUTF8;
begin
  Result := GetValueAt(IndexOfName(Name));
end;

function TRawUTF8List.GetValueAt(Index: PtrInt): RawUTF8;
var Sep: PUTF8Char;
begin
  if (self=nil) or (PtrUInt(Index)>=PtrUInt(fCount)) then
    result := '' else
    result := Get(Index);
  if result='' then
    exit;
  Sep := PosChar(pointer(result),NameValueSep);
  if Sep=nil then
    result := '' else
    result := Sep+1; // get 'Value' from 'Name=Value'
end;

function TRawUTF8List.IndexOf(const aText: RawUTF8): PtrInt;
begin
  if self<>nil then
    for result := 0 to fCount-1 do
      if fList[result]=aText then
        exit;
  result := -1;
end;

function TRawUTF8List.IndexOfName(const Name: RawUTF8): PtrInt;
var UpperName: array[byte] of AnsiChar;
begin
  if self<>nil then begin
    PWord(UpperCopy255(UpperName,Name))^ := ord(NameValueSep);
    for result := 0 to fCount-1 do
      if IdemPChar(Pointer(fList[result]),UpperName) then
        exit;
  end;
  result := -1;
end;

function TRawUTF8List.IndexOfObject(aObject: TObject): PtrInt;
begin
  if (self<>nil) and (fObjects<>nil) then
{$ifdef CPU64}
    for result := 0 to fCount-1 do
      if fObjects[result]=aObject then
        exit;
{$else}
    result := IntegerScanIndex(pointer(fObjects),fCount,cardinal(aObject)) else
{$endif}
  result := -1;
end;

procedure TRawUTF8List.OnChangeHidden(Sender: TObject);
begin
  if self<>nil then
    fOnChangeTrigerred := true;
end;

procedure TRawUTF8List.Put(Index: PtrInt; const Value: RawUTF8);
begin
  if (self<>nil) and (PtrUInt(Index)<PtrUInt(fCount)) then begin
    fList[Index] := Value;
    Changed;
  end;
end;

procedure TRawUTF8List.PutObject(Index: PtrInt; const Value: TObject);
begin
  if (self<>nil) and (PtrUInt(Index)<PtrUInt(fCount)) then begin
    if fObjects=nil then
      SetLength(fObjects,Length(fList));
    fObjects[Index] := Value;
    Changed;
  end;
end;

procedure TRawUTF8List.SetCapacity(const Value: PtrInt);
begin
  if self<>nil then begin
    if Value<=0 then begin
      SetLength(fList,0);
      SetLength(fObjects,0);
      fCount := 0;
    end else begin
      if Value<fCount then
        fCount := Value;
      if Value>length(fList) then begin
        SetLength(fList,Value);
        if pointer(fObjects)<>nil then
          SetLength(fObjects,Value);
      end;
    end;
  end;
end;

procedure TRawUTF8List.SetText(const aText, Delimiter: RawUTF8);
begin
  SetTextPtr(pointer(aText),Delimiter);
end;

procedure TRawUTF8List.LoadFromFile(const FileName: TFileName);
var Map: TMemoryMap;
    P: pointer;
begin
  if Map.Map(FileName) then
  try
    if Map.Size<>0 then begin
      P := Map.Buffer;
      if (Map.Size>3) and (PCardinal(P)^ and $ffffff=$bfbbef) then
        inc(PByte(P),3); // ignore UTF-8 BOM
      SetTextPtr(P,#13#10);
    end;
  finally
    Map.UnMap;
  end;
end;

procedure TRawUTF8List.SetTextPtr(P: PUTF8Char; const Delimiter: RawUTF8);
var DelimLen: PtrInt;
    DelimFirst: AnsiChar;
    PDeb, DelimNext: PUTF8Char;
    Line: RawUTF8;
begin
  DelimLen := length(Delimiter);
  BeginUpdate;
  Clear;
  if DelimLen>0 then begin
    DelimFirst := Delimiter[1];
    DelimNext := PUTF8Char(pointer(Delimiter))+1;
    repeat
      PDeb := P;
      while P^<>#0 do begin
        if (P^=DelimFirst) and CompareMem(P+1,DelimNext,DelimLen-1) then
          break;
        inc(P);
      end;
      SetString(Line,PDeb,P-PDeb);
      AddObject(Line,nil);
      if P^=#0 then
        break;
      inc(P,DelimLen);
    until P^=#0;
  end;
  EndUpdate;
end;

procedure TRawUTF8List.SetTextCRLF(const Value: RawUTF8);
begin
  SetTextPtr(pointer(Value),#13#10);
end;

procedure TRawUTF8List.SetValue(const Name, Value: RawUTF8);
var i: PtrInt;
begin
  i := IndexOfName(Name);
  if i<0 then
    Add(Name+RawUTF8(NameValueSep)+Value) else
    fList[i] := Name+RawUTF8(NameValueSep)+Value;
end;

procedure TRawUTF8List.UpdateValue(const Name: RawUTF8; var Value: RawUTF8;
  ThenDelete: boolean);
var i: PtrInt;
begin
  i := IndexOfName(Name);
  if i>=0 then begin
    Value := GetValueAt(i); // update value
    if ThenDelete then
      Delete(i); // optionally delete
  end;
end;


{ TObjectListHashedAbstract}

constructor TObjectListHashedAbstract.Create(aFreeItems: boolean);
begin
  inherited Create;
  fFreeItems := aFreeItems;
  fHash.Init(TypeInfo(TObjectDynArray),fList,@HashPtrUInt,@SortDynArrayPointer,nil,@fCount);
end;

destructor TObjectListHashedAbstract.Destroy;
var i: integer;
begin
  if fFreeItems then
    for i := 0 to fCount-1 do
      List[i].Free;
  inherited;
end;

procedure TObjectListHashedAbstract.Delete(aIndex: integer);
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) then
    exit;
  if fFreeItems then
    List[aIndex].Free;
  fHash.Delete(aIndex);
  fHashValid := false;
end;

procedure TObjectListHashedAbstract.Delete(aObject: TObject);
begin
  Delete(IndexOf(aObject));
end;

const
  // hashing will start only when List[] reachs 32 items (not worth it before)
  TOBJECTLISTHASHED_START_HASHING_COUNT = 32;

{ TObjectListHashed }

function TObjectListHashed.Add(aObject: TObject; out wasAdded: boolean): integer;
begin
  wasAdded := false;
  if self<>nil then
    if fHashed then begin
      if not fHashValid then begin
        fHash.ReHash;
        fHashValid := true;
      end;
      result := fHash.FindHashedForAdding(aObject,wasAdded);
      if wasAdded then
        fList[result] := aObject;
    end else begin
      for result := 0 to fCount-1 do 
        if fList[result]=aObject then
          exit;
      wasAdded := true;
      result := fHash.Add(aObject);
      if fCount>=TOBJECTLISTHASHED_START_HASHING_COUNT then
        fHashed := true;    
    end
  else
    result := -1;
end;

function TObjectListHashed.IndexOf(aObject: TObject): integer;
begin
  if (self<>nil) and (fCount>0) then 
    if fHashed then begin
      if not fHashValid then begin
        fHash.ReHash;
        fHashValid := true;
      end;
      result := fHash.FindHashed(aObject);
      exit;
    end else
    for result := 0 to fCount-1 do
      if fList[result]=aObject then
        exit;
  result := -1;
end;


{ TObjectListPropertyHashed }

constructor TObjectListPropertyHashed.Create(
  aSubPropAccess: TObjectListPropertyHashedAccessProp;
  aHashElement: TDynArrayHashOne; aCompare: TDynArraySortCompare;
  aFreeItems: boolean);
begin
  inherited Create(aFreeItems);
  fSubPropAccess := aSubPropAccess;
  if @aHashElement<>nil then
    fHash.fHashElement := aHashElement;
  if @aCompare<>nil then
    fHash.fCompare := aCompare;
  fHash.EventCompare := IntComp;
end;

function TObjectListPropertyHashed.IntHash(const Elem): cardinal;
var oObject: TObject;
begin
  oObject := fSubPropAccess(TObject(Elem));
  result := fHash.fHashElement(oObject,fHash.fHasher);
end;

function TObjectListPropertyHashed.IntComp(const A,B): integer;
var O: TObject;
begin
  O := fSubPropAccess(TObject(A));
  result := fHash.fCompare(O,B);
end;

procedure TObjectListPropertyHashed.IntHashValid;
begin
  fHash.ReHash(IntHash);
  fHashValid := true;
end;

function TObjectListPropertyHashed.Add(aObject: TObject; out wasAdded: boolean): integer;
begin
  wasAdded := false;
  if self<>nil then
    if fHashed then begin
      if not fHashValid then 
        IntHashValid;
      result := fHash.FindHashedForAdding(aObject,wasAdded,
        fHash.fHashElement(aObject,fHash.fHasher));
      if wasAdded then
        fList[result] := aObject;
    end else begin
      for result := 0 to fCount-1 do
        if IntComp(fList[result],aObject)=0 then
          exit;
      wasAdded := true;
      result := fHash.Add(aObject);
      if fCount>=TOBJECTLISTHASHED_START_HASHING_COUNT then
        fHashed := true;    
    end
  else
    result := -1;
end;

function TObjectListPropertyHashed.IndexOf(aObject: TObject): integer;
begin
  if (self<>nil) and (fCount>0) then 
    if fHashed then begin
      if not fHashValid then
        IntHashValid;
      result := fHash.HashFind(fHash.fHashElement(aObject,fHash.fHasher),PtrInt(aObject));
      if result>=0 then
        exit; // if found
    end else
    for result := 0 to fCount-1 do
      if IntComp(fList[result],aObject)=0 then
        exit;
  result := -1;
end;


{ TRawUTF8ListHashed }

constructor TRawUTF8ListHashed.Create;
begin
  inherited;
  fHash.Init(TypeInfo(TRawUTF8DynArray),fList,nil,nil,nil,@fCount);
end;

procedure TRawUTF8ListHashed.Changed;
begin
  fChanged := true;
  inherited;
end;

procedure TRawUTF8ListHashed.SetHashCaseInsensitive(Value: boolean);
begin
  if fHashCaseInsensitive=Value then
    exit;
  fHashCaseInsensitive := Value;
  fHash.fHashElement := HASHFIRSTFIELD[Value,djRawUTF8];
  fHash.fCompare := SORTFIRSTFIELD[Value,djRawUTF8];
  fChanged := true;
end;

function TRawUTF8ListHashed.IndexOf(const aText: RawUTF8): PtrInt;
begin
  if fChanged then begin
    fHash.ReHash; // rough, but working implementation
    fChanged := false;
  end;
  result := fHash.FindHashed(aText);
end;


{ TMemoryMap }

function TMemoryMap.Map(aFile: THandle; aCustomSize: cardinal; aCustomOffset: Int64): boolean;
var Available: Int64;
begin
  result := false;
  fBuf := nil;
  fMap := 0;
  fFileLocal := false;
  fFile := aFile;
  fFileSize := FileSeek64(fFile,0,soFromEnd);
  if (fFileSize<=0) or (fFileSize>maxInt) then
    /// maxInt = $7FFFFFFF = 1.999 GB (2GB would induce PtrInt errors)
    exit;
  if aCustomSize=0 then
    fBufSize := Int64Rec(fFileSize).Lo else begin
    Available := fFileSize-aCustomOffset;
    if Available<0 then
      exit;
    if aCustomSize>Available then
      fBufSize := Int64Rec(Available).Lo;
      fBufSize := aCustomSize;
  end;
  with Int64Rec(fFileSize) do
    fMap := CreateFileMapping(fFile,nil,PAGE_READONLY,Hi,Lo,nil);
  if fMap=0 then
    raise ESynException.Create('MemoryMap.Map');
  with Int64Rec(aCustomOffset) do
    fBuf := MapViewOfFile(fMap,FILE_MAP_READ,Hi,Lo,fBufSize);
  if fBuf=nil then begin
    // Windows failed to find a contiguous VA space -> fall back on direct read
    CloseHandle(fMap);
    fMap := 0;
  end else
    result := true;
end;

procedure TMemoryMap.Map(aBuffer: pointer; aBufferSize: cardinal);
begin
  fBuf := aBuffer;
  fFileSize := aBufferSize;
  fBufSize := aBufferSize;
  fMap := 0;
  fFile := 0;
  fFileLocal := false;
end;

function TMemoryMap.Map(const aFileName: TFileName): boolean;
var F: THandle;
begin
  result := false;
  F := FileOpen(aFileName,fmOpenRead or fmShareDenyNone);
  if F=INVALID_HANDLE_VALUE then
    exit;
  if Map(F)  then
    result := true else
    FileClose(F);
  fFileLocal := result;
end;

procedure TMemoryMap.UnMap;
begin
  if fMap<>0 then begin
    UnmapViewOfFile(fBuf);
    CloseHandle(fMap);
    fMap := 0;
    if fFileLocal then
      FileClose(fFile);
    fFile := 0;
  end;
end;


{ TSynMemoryStream }

constructor TSynMemoryStream.Create(const aText: RawByteString);
begin
  inherited Create;
  SetPointer(pointer(aText),length(aText));
end;

constructor TSynMemoryStream.Create(Data: pointer; DataLen: integer);
begin
  inherited Create;
  SetPointer(Data,DataLen);
end;

function TSynMemoryStream.Write(const Buffer; Count: Integer): Longint;
begin
  raise EStreamError.Create('TSynMemoryStream.Write');
end;


{ TSynMemoryStreamMapped }

constructor TSynMemoryStreamMapped.Create(const aFileName: TFileName; aCustomSize: cardinal; aCustomOffset: Int64);
begin
  fFileStream := TFileStream.Create(aFileName,fmOpenRead or fmShareDenyNone);
  Create(fFileStream.Handle);
end;

constructor TSynMemoryStreamMapped.Create(aFile: THandle; aCustomSize: cardinal; aCustomOffset: Int64);
begin
  if not fMap.Map(aFile) then
    raise ESynException.CreateFmt('%s mapping error',[ClassName]);
  inherited Create(fMap.fBuf,fMap.fBufSize);
end;

destructor TSynMemoryStreamMapped.Destroy;
begin
  fMap.UnMap;
  fFileStream.Free;
  inherited;
end;



function FileSeek64(Handle: THandle; const Offset: Int64; Origin: DWORD): Int64;
var R64: packed record Lo, Hi: integer; end absolute Result;
begin
  Result := Offset;
  R64.Lo := integer(SetFilePointer(Handle,R64.Lo,@R64.Hi,Origin));
  if (R64.Lo=-1) and (GetLastError<>0) then
    R64.Hi := -1; // so result=-1
end;


{ TFileBufferWriter }

constructor TFileBufferWriter.Create(aFile: THandle; BufLen: integer);
begin
  Create(THandleStream.Create(aFile),BufLen);
  fInternalStream := true;
end;

constructor TFileBufferWriter.Create(const aFileName: TFileName; BufLen: integer);
begin
  Create(TFileStream.Create(aFileName,fmCreate),BufLen);
  fInternalStream := true;
end;

constructor TFileBufferWriter.Create(aStream: TStream; BufLen: integer);
begin
  if BufLen>1 shl 22 then
    BufLen := 1 shl 22; // 4 MB sounds right enough
  fBufLen := BufLen;
  fStream := aStream;
  SetLength(fBuf,fBufLen);
end;

constructor TFileBufferWriter.CreateInMemoryStream;
begin
  Create(THeapMemoryStream.Create,4096);
  fInternalStream := true;
end;

destructor TFileBufferWriter.Destroy;
begin
  if fInternalStream then
    fStream.Free;
  inherited;
end;

function TFileBufferWriter.Flush: Int64;
begin
  if fPos>0 then begin
    fStream.Write(pointer(fBuf)^,fPos);
    fPos := 0;
  end;
  result := fTotalWritten;
  fTotalWritten := 0;
end;

procedure TFileBufferWriter.Write(Data: pointer; DataLen: integer);
begin
  if DataLen<=0 then
    exit;
  if fPos+DataLen>fBufLen then begin
    if fPos>0 then begin
      fStream.Write(pointer(fBuf)^,fPos);
      fPos := 0;
    end;
    if DataLen>fBufLen then begin
      fStream.Write(Data^,DataLen);
      inc(fTotalWritten,PtrUInt(DataLen));
      exit;
    end;
  end;
  move(Data^,PByteArray(fBuf)^[fPos],DataLen);
  inc(fPos,DataLen);
  inc(fTotalWritten,PtrUInt(DataLen));
end;

procedure TFileBufferWriter.Write(const Text: RawUTF8);
var L: integer;
begin
  L := length(Text);
  WriteVarUInt32(L);
  if L=0 then
    exit;
  Write(pointer(Text),L);
end;

procedure TFileBufferWriter.WriteRawUTF8DynArray(const Values: TRawUTF8DynArray;
  ValuesCount: integer);
var PI: PIntegerArray;
    n, i, fixedsize: integer;
    len: PtrUInt;
    P, PEnd: PByte;
    PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  PI := pointer(Values);
  if ValuesCount=0 then
    exit;
  fixedsize := length(Values[0]);
  if fixedsize>0 then
    for i := 1 to ValuesCount-1 do
      if (PI^[i]=0) or (PInteger(PI^[i]-4)^<>fixedsize) then begin
        fixedsize := 0;
        break;
      end;
  WriteVarUInt32(fixedsize);
  repeat
    P := @PByteArray(fBuf)^[fPos];
    PEnd := @PByteArray(fBuf)^[fBufLen-8];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      n := ValuesCount;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P,4);
      if fixedsize=0 then
      for i := 0 to ValuesCount-1 do
        if PI^[i]=0 then begin
          P^ := 0; // store length=0
          inc(P);
          if PtrUInt(P)>=PtrUInt(PEnd) then begin
            n := i+1;
            break; // avoid buffer overflow
          end;
        end else begin
          len := PInteger(PI^[i]-4)^; // fast length(Values[])
          if PtrUInt(P)+len>=PtrUInt(PEnd) then begin
            n := i;
            break; // avoid buffer overflow
          end;
          P := ToVarUInt32(len,P);
          Move(pointer(PI^[i])^,P^,len);
          inc(P,len);
        end else
      // fixed size strings case
      for i := 0 to ValuesCount-1 do begin
        if PtrUInt(P)+PtrUInt(fixedsize)>=PtrUInt(PEnd) then begin
          n := i;
          break; // avoid buffer overflow
        end;
        Move(pointer(PI^[i])^,P^,fixedsize);
        inc(P,fixedsize);
      end;
      len := PAnsiChar(P)-PBeg; // format: Isize+varUInt32s*strings
      PInteger(PBeg)^ := len-4;
      Inc(fTotalWritten,len);
      inc(fPos,len);
      inc(PtrUInt(PI),n*4);
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    fStream.Write(pointer(fBuf)^,fPos);
    fPos := 0;
  until false;
end;

procedure TFileBufferWriter.WriteRawUTF8List(List: TRawUTF8List;
  StoreObjectsAsVarUInt32: Boolean);
var i: integer;
begin
  if List=nil then
    WriteVarUInt32(0) else begin
    WriteRawUTF8DynArray(List.fList,List.Count);
    if List.fObjects=nil then
      StoreObjectsAsVarUInt32 := false; // no Objects[] values
    Write(@StoreObjectsAsVarUInt32,1);
    if StoreObjectsAsVarUInt32 then
      for i := 0 to List.fCount-1 do
        WriteVarUInt32(PtrUInt(List.fObjects[i]));
  end;
end;

procedure TFileBufferWriter.WriteStream(aStream: TCustomMemoryStream;
  aStreamSize: Integer);
begin
  if aStreamSize<0 then
    if aStream=nil then
      aStreamSize := 0 else
      aStreamSize := aStream.Size;
  WriteVarUInt32(aStreamSize);
  if aStreamSize>0 then
    Write(aStream.Memory,aStreamSize);
end;

procedure TFileBufferWriter.WriteVarInt32(Value: PtrInt);
begin
  if Value<=0 then
    // 0->0, -1->2, -2->4..
    Value := (-Value) shl 1 else
    // 1->1, 2->3..
    Value := (Value shl 1)-1;
  WriteVarUInt32(Value);
end;

procedure TFileBufferWriter.WriteVarUInt32(Value: PtrUInt);
var pos: integer;
begin
  if fPos+16>fBufLen then begin
    fStream.Write(pointer(fBuf)^,fPos);
    fPos := 0;
  end;
  pos := fPos;
  fPos := PtrUInt(ToVarUInt32(Value,@PByteArray(fBuf)^[fPos]))-PtrUInt(fBuf);
  inc(fTotalWritten,PtrUInt(fPos-Pos));
end;

function CleverStoreInteger(p: pInteger; V, VEnd: PAnsiChar; pCount: integer;
  var StoredCount: integer): PAnsiChar;
// Clever = store Values[i+1]-Values[i] (with special diff=1 count)
// format:  Integer: firstValue, then:
//          B:0 W:difference with previous
//          B:1..253 = difference with previous
//          B:254 W:byOne
//          B:255 B:byOne
var i, d, byOne: integer;
begin
  StoredCount := pCount;
  if pCount<=0 then begin
    result := V;
    exit;
  end;
  i := p^;
  pInteger(V)^ := p^;
  inc(V,4);
  dec(pCount);
  inc(p);
  byOne := 0;
  if pCount>0 then
  repeat
    d := p^-i;
    i := p^;
    inc(p);
    if d=1 then begin
      dec(pCount);
      inc(byOne);
      if pCount>0 then continue;
    end else
    if d<0 then begin
      result:= nil;
      exit;
    end;
    if byOne<>0 then begin
      case byOne of
      1: begin V^ := #1; inc(V); end; // B:1..253 = difference with previous
      2: begin pWord(V)^ := $0101; inc(V,2); end; // B:1..253 = difference
      else
      if byOne>255 then begin
        while byOne>65535 do begin
          pInteger(V)^ := $fffffe; inc(V,3); // store as many len=$ffff as necessary
          dec(byOne,$ffff);
        end;
        pInteger(V)^ := byOne shl 8+$fe; inc(V,3); // B:254 W:byOne
      end else begin
        pWord(V)^ := byOne shl 8+$ff; inc(V,2); // B:255 B:byOne
      end;
      end; // case byOne of
      if pCount=0 then break;
      byOne := 0;
    end;
    if (d=0) or (d>253) then begin
      while cardinal(d)>65535 do begin
        pInteger(V)^ := $ffff00; inc(V,3); // store as many len=$ffff as necessary
        dec(cardinal(d),$ffff);
      end;
      dec(pCount);
      pInteger(V)^ := d shl 8; inc(V,3); // B:0 W:difference with previous
      if (V<VEnd) and (pCount>0) then continue else break;
    end else begin
      dec(pCount);
      V^ := AnsiChar(d); inc(V); // B:1..253 = difference with previous
      if (V<VEnd) and (pCount>0) then continue else break;
    end;
    if V>=VEnd then
      break; // avoid GPF
  until false;
  dec(StoredCount,pCount);
  result := V;
end;

procedure TFileBufferWriter.WriteVarUInt32Array(const Values: TIntegerDynArray;
  ValuesCount: integer; DataLayout: TFileBufferWriterKind);
var n, i, pos, diff: integer;
    P: PByte;
    PI: PIntegerArray;
    PBeg, PEnd: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount=0 then
    exit;
  PI := pointer(Values);
  PByteArray(fBuf)^[fPos] := ord(DataLayout);
  inc(fPos);
  inc(fTotalWritten);
  if DataLayout in [wkOffsetU, wkOffsetI] then begin
    pos := fPos;
    fPos := PtrUInt(ToVarUInt32(PI^[0],@PByteArray(fBuf)^[fPos]))-PtrUInt(fBuf);
    diff := PI^[1]-PI^[0];
    inc(PtrUInt(PI),4);
    dec(ValuesCount);
    if ValuesCount=0 then begin
      inc(fTotalWritten,PtrUInt(fPos-pos));
      exit;
    end;
    if diff>0 then begin
      for i := 1 to ValuesCount-1 do
        if PI^[i]-PI^[i-1]<>diff then begin
          diff := 0; // not always the same offset
          break;
        end;
    end else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff,@PByteArray(fBuf)^[fPos]))-PtrUInt(fBuf);
    inc(fTotalWritten,PtrUInt(fPos-pos));
    if diff<>0 then
      exit; // same offset for all items (fixed sized records) -> quit now
  end;
  repeat
    P := @PByteArray(fBuf)^[fPos];
    PEnd := @PByteArray(fBuf)^[fBufLen-32];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      pos := fPos;
      case DataLayout of
      wkUInt32: begin
        n := (fBufLen-fPos)shr 2;
        if ValuesCount<n then
          n := ValuesCount;
        Move(PI^,P^,n*4);
        inc(P,n*4);
      end;
      wkVarInt32, wkVarUInt32, wkOffsetU, wkOffsetI: begin
        PBeg := PAnsiChar(P); // leave space for chunk size
        inc(P,4);
        n := ValuesCount;
        case DataLayout of
        wkVarInt32:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarInt32(PI^[i],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkVarUInt32:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarUInt32(PI^[i],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkOffsetU:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarUInt32(PI^[i]-PI^[i-1],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        wkOffsetI:
          for i := 0 to ValuesCount-1 do begin
            P := ToVarInt32(PI^[i]-PI^[i-1],P);
            if PtrUInt(P)>=PtrUInt(PEnd) then begin
              n := i+1;
              break; // avoid buffer overflow
            end;
          end;
        end;
        PInteger(PBeg)^ := PAnsiChar(P)-PBeg-4; // format: Isize+varUInt32s
      end;
      wkSorted: begin
        PBeg := PAnsiChar(P)+4; // leave space for chunk size
        P := PByte(CleverStoreInteger(pointer(PI),PBeg,PEnd,ValuesCount,n));
        if P=nil then
          raise ESynException.Create('WriteVarUInt32Array: data not sorted');
        PInteger(PBeg-4)^ := PAnsiChar(P)-PBeg; // format: Isize+cleverStorage
      end;
      end;
      inc(PtrUInt(PI),n*4);
      fPos := PtrUInt(P)-PtrUInt(fBuf);
      inc(fTotalWritten,PtrUInt(fPos-pos));
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    fStream.Write(pointer(fBuf)^,fPos);
    fPos := 0;
  until false;
end;

procedure TFileBufferWriter.WriteVarUInt64DynArray(
  const Values: TInt64DynArray; ValuesCount: integer; Offset: Boolean);
var n, i, pos, diff: integer;
    P, PEnd: PByte;
    PI: PInt64Array;
    PBeg: PAnsiChar;
begin
  WriteVarUInt32(ValuesCount);
  if ValuesCount=0 then
    exit;
  PI := pointer(Values);
  pos := fPos;
  if Offset then begin
    PByteArray(fBuf)^[fPos] := 1;
    fPos := PtrUInt(ToVarUInt64(PI^[0],@PByteArray(fBuf)^[fPos+1]))-PtrUInt(fBuf);
    diff := PI^[1]-PI^[0];
    inc(PtrUInt(PI),8);
    dec(ValuesCount);
    if ValuesCount=0 then begin
      inc(fTotalWritten,PtrUInt(fPos-pos));
      exit;
    end;
    if diff>0 then begin
      for i := 1 to ValuesCount-1 do
        if PtrInt(PI^[i]-PI^[i-1])<>diff then begin
          diff := 0; // not always the same offset
          break;
        end;
    end else
      diff := 0;
    fPos := PtrUInt(ToVarUInt32(diff,@PByteArray(fBuf)^[fPos]))-PtrUInt(fBuf);
    if diff<>0 then begin
      inc(fTotalWritten,PtrUInt(fPos-Pos));
      exit; // same offset for all items (fixed sized records) -> quit now
    end;
  end else begin
    PByteArray(fBuf)^[fPos] := 0;
    inc(fPos);
  end;
  inc(fTotalWritten,PtrUInt(fPos-Pos));
  repeat
    P := @PByteArray(fBuf)^[fPos];
    PEnd := @PByteArray(fBuf)^[fBufLen-32];
    if PtrUInt(P)<PtrUInt(PEnd) then begin
      pos := fPos;
      PBeg := PAnsiChar(P); // leave space for chunk size
      inc(P,4);
      n := ValuesCount;
      if Offset then
        for i := 0 to ValuesCount-1 do begin
          P := ToVarUInt32(PI^[i]-PI^[i-1],P); // store diffs
          if PtrUInt(P)>=PtrUInt(PEnd) then begin
            n := i+1;
            break; // avoid buffer overflow
          end;
        end
      else
        for i := 0 to ValuesCount-1 do begin
          P := ToVarUInt64(PI^[i],P);
          if PtrUInt(P)>=PtrUInt(PEnd) then begin
            n := i+1;
            break; // avoid buffer overflow
          end;
        end;
      PInteger(PBeg)^ := PAnsiChar(P)-PBeg-4; // format: Isize+varUInt32/64s
      inc(PtrUInt(PI),n*8);
      fPos := PtrUInt(P)-PtrUInt(fBuf);
      inc(fTotalWritten,PtrUInt(fPos-Pos));
      dec(ValuesCount,n);
      if ValuesCount=0 then
        break;
    end;
    fStream.Write(pointer(fBuf)^,fPos);
    fPos := 0;
  until false;
end;


{ TFileBufferReader }

procedure TFileBufferReader.Close;
begin
  fMap.UnMap;
end;

procedure TFileBufferReader.ErrorInvalidContent;
begin
  raise ESynException.Create('TFileBufferReader: invalid content');
end;

procedure TFileBufferReader.OpenFrom(aBuffer: pointer; aBufferSize: cardinal);
begin
  fCurrentPos := 0;
  fMap.Map(aBuffer,aBufferSize);
end;

function TFileBufferReader.OpenFrom(Stream: TStream): boolean;
begin
  result := false;
  if Stream=nil then
    exit;
  if Stream.InheritsFrom(TFileStream) then
    Open(TFileStream(Stream).Handle) else
  if Stream.InheritsFrom(TCustomMemoryStream) then
    with TCustomMemoryStream(Stream) do
      OpenFrom(Memory,Size) else
      exit;
  result := true
end;

procedure TFileBufferReader.Open(aFile: THandle);
begin
  fCurrentPos := 0;
  fMap.Map(aFile)
  // if Windows failed to find a contiguous VA space -> fall back on direct read
end;

function TFileBufferReader.Read(Data: pointer; DataLen: integer): integer;
var len: integer;
begin
  if DataLen>0 then
    if fMap.fBuf<>nil then begin
      // file up to 2 GB: use fast memory map
      len := fMap.fBufSize-fCurrentPos;
      if len>DataLen then
        len := DataLen;
      move(fMap.fBuf[fCurrentPos],Data^,len);
      inc(fCurrentPos,len);
      result := len;
    end else
      // file bigger than 2 GB: slower but accurate reading from file
      result := FileRead(fMap.fFile,Data^,DataLen) else
      // DataLen=0
      result := 0;
end;

function TFileBufferReader.Read(out Text: RawUTF8): integer;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  SetLength(Text,result);
  if Read(pointer(Text),result)<>result then
    ErrorInvalidContent;
end;

function TFileBufferReader.ReadRawUTF8: RawUTF8;
begin
  Read(result);
end;

procedure TFileBufferReader.ReadChunk(var P, PEnd: PByte; var BufTemp: RawByteString);
var len: integer;
begin // read Isize + buffer in P,PEnd
  if (Read(@len,4)<>4) or (len<0) then
    ErrorInvalidContent;
  P := ReadPointer(len,BufTemp);
  if P=nil then
    ErrorInvalidContent;
  PEnd := pointer(PtrUInt(P)+PtrUInt(len));
end;

function TFileBufferReader.CurrentMemory: pointer;
begin
  if (fMap.fBuf=nil) or (fCurrentPos>=fMap.fBufSize) then
    result := nil else
    result := @fMap.fBuf[fCurrentPos];
end;

function TFileBufferReader.ReadPointer(DataLen: PtrUInt;
  var aTempData: RawByteString): pointer;
begin
  if fMap.fBuf=nil then begin
    // read from file
    if DataLen>PtrUInt(Length(aTempData)) then begin
      aTempData := ''; // so no move() call in SetLength() below
      SetLength(aTempData,DataLen);
    end;
    if PtrUInt(FileRead(fMap.fFile,pointer(aTempData)^,DataLen))<>DataLen then
      result := nil else // invalid content
      result := pointer(aTempData);
  end else
  if DataLen+fCurrentPos>fMap.fBufSize then
    // invalid request
    result := nil else begin
    // get pointer to data from current memory map (no data copy)
    result := @fMap.fBuf[fCurrentPos];
    inc(fCurrentPos,DataLen);
  end;
end;

function TFileBufferReader.ReadStream(DataLen: PtrInt): TCustomMemoryStream;
var FileCurrentPos: Int64;
begin
  if DataLen<0 then
    DataLen := ReadVarUInt32;
  if DataLen<>0 then
    if fMap.fBuf=nil then begin
      FileCurrentPos := FileSeek64(fMap.fFile,0,soFromCurrent);
      if FileCurrentPos+DataLen>fMap.fFileSize then
        // invalid content
        result := nil else begin
        // create a temporary memory map buffer stream
        result := TSynMemoryStreamMapped.Create(fMap.fFile,DataLen,FileCurrentPos);
        FileSeek64(fMap.fFile,DataLen,soFromCurrent);
      end;
    end else
    if PtrUInt(DataLen)+fCurrentPos>fMap.fBufSize then
      // invalid content
      result := nil else begin
      // get pointer to data from current memory map (no data copy)
      result := TSynMemoryStream.Create(@fMap.fBuf[fCurrentPos],DataLen);
      inc(fCurrentPos,DataLen);
    end else
    // DataLen=0 -> invalid content
    result := nil;
end;

function TFileBufferReader.ReadByte: PtrUInt;
begin
  if fMap.fBuf<>nil then
    if fCurrentPos>=fMap.fBufSize then
      // invalid request
      result := 0 else begin
      // read one byte from current memory map
      result := ord(fMap.fBuf[fCurrentPos]);
      inc(fCurrentPos);
    end else begin
    // read from file if >= 2 GB (slow, but works)
    result := 0;
    if FileRead(fMap.fFile,result,1)<>1 then
      result := 0;
  end;
end;

function TFileBufferReader.ReadCardinal: cardinal;
begin
  if fMap.fBuf<>nil then
    if fCurrentPos+3>=fMap.fBufSize then
      // invalid request
      result := 0 else begin
      // read one byte from current memory map
      result := PCardinal(fMap.fBuf+fCurrentPos)^;
      inc(fCurrentPos,4);
    end else begin
      // read from file if >= 2 GB (slow, but works)
      result := 0;
      if FileRead(fMap.fFile,result,4)<>4 then
        result := 0;
    end;
end;

function TFileBufferReader.ReadVarUInt32: PtrUInt;
var c, n: PtrUInt;
begin
  result := ReadByte;
  if result>$7f then begin
    n := 0;
    result := result and $7F;
    repeat
      c := ReadByte;
      inc(n,7);
      if c<=$7f then break;
      result := result or ((c and $7f) shl n);
    until false;
    result := result or (c shl n);
  end;
end;

function TFileBufferReader.ReadVarInt32: PtrInt;
begin
  result := ReadVarUInt32;
  if result and 1<>0 then
    // 1->1, 3->2..
    result := result shr 1+1 else
    // 0->0, 2->-1, 4->-2..
    result := -(result shr 1);
end;

function TFileBufferReader.ReadVarUInt64: QWord;
var c, n: PtrUInt;
begin
  result := ReadByte;
  if result>$7f then begin
    n := 0;
    result := result and $7F;
    repeat
      c := ReadByte;
      inc(n,7);
      if c<=$7f then break;
      result := result or ((c and $7f) shl n);
    until false;
    result := result or (c shl n);
  end;
end;

function CleverReadInteger(p, pEnd: PAnsiChar; V: pInteger): PtrUInt;
// Clever = decode Values[i+1]-Values[i] storage (with special diff=1 count)
var i, n: PtrUInt;
begin
  result := PtrUInt(V);
  i := pInteger(p)^; inc(p,4); // Integer: firstValue
  V^ := i; inc(V);
  if PtrUInt(p)<PtrUInt(pEnd) then
  repeat
    case p^ of
      #0: begin // B:0 W:difference with previous
        inc(i,pWord(p+1)^); inc(p,3);
        V^ := i; inc(V);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
      #254: begin // B:254 W:byOne
        for n := 1 to pWord(p+1)^ do begin
          inc(i);
          V^ := i; inc(V);
        end;
        inc(p,3);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
      #255: begin // B:255 B:byOne
        for n := 1 to pByte(p+1)^ do begin
          inc(i);
          V^ := i; inc(V);
        end;
        inc(p,2);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end else begin // B:1..253 = difference with previous
        inc(i,ord(p^)); inc(p);
        V^ := i; inc(V);
        if PtrUInt(p)<PtrUInt(pEnd) then continue else break;
      end;
    end; // case p^ of
  until false;
  result := (PtrUInt(V)-result) shr 2; // returns count of stored integer 
end;

function TFileBufferReader.ReadVarUInt32Array(var Values: TIntegerDynArray): PtrInt;
var count, n, i, diff: integer;
    DataLayout: TFileBufferWriterKind;
    P, PEnd: PByte;
    PI: PInteger;
    PIA: PIntegerArray absolute PI;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  count := result;
  if count>length(Values) then // only set length is not big enough
    SetLength(Values,count);
  PI := pointer(Values);
  DataLayout := TFileBufferWriterKind(ReadByte);
  if DataLayout in [wkOffsetU, wkOffsetI] then begin
    PI^ := ReadVarUInt32;
    dec(count);
    if count=0 then
      exit;
    diff := ReadVarUInt32;
    if diff<>0 then begin
      for i := 0 to count-1 do
        PIA^[i+1] := PIA^[i]+diff;
      exit;
    end;
  end;
  if DataLayout=wkUint32 then
    Read(@Values[0],count*4) else begin
    repeat
      ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
      case DataLayout of
      wkVarInt32:
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PI^ := FromVarInt32(P);
          dec(count);
          inc(PI);
        end;
      wkVarUInt32:
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PI^ := FromVarUInt32(P);
          dec(count);
          inc(PI);
        end;
      wkSorted: begin
        n := CleverReadInteger(pointer(P),pointer(PEnd),PI);
        dec(count,n);
        inc(PtrUInt(PI),n*4);
      end;
      wkOffsetU: begin
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PIA^[1] := PIA^[0]+integer(FromVarUInt32(P));
          dec(count);
          inc(PI);
        end;
        if count<=0 then
          inc(PI); // make sure PI=@Values[result]
      end;
      wkOffsetI: begin
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PIA^[1] := PIA^[0]+FromVarInt32(P);
          dec(count);
          inc(PI);
        end;
        if count<=0 then
          inc(PI); // make sure PI=@Values[result]
      end;
      else
        ErrorInvalidContent;
      end;
    until count<=0;
    if PI<>@Values[result] then
      ErrorInvalidContent;
  end;
end;

function TFileBufferReader.ReadVarUInt64Array(var Values: TInt64DynArray): PtrInt;
var count, diff, i: integer;
    Offset: boolean;
    P, PEnd: PByte;
    PI: PInt64;
    PIA: PInt64Array absolute PI;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  count := result;
  if count>length(Values) then // only set length is not big enough
    SetLength(Values,count);
  Offset := boolean(ReadByte);
  PI := pointer(Values);
  if Offset then begin
    PI^ := ReadVarUInt64; // read first value
    dec(count);
    diff := ReadVarUInt32;
    if diff=0 then begin
      // read all offsets, and compute (not fixed sized records)
      repeat
        ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
        while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
          PIA^[1] := PIA^[0]+integer(FromVarUInt32(P)); 
          dec(count);
          inc(PI);
        end;
      until count<=0;
      {$ifopt C+} inc(PI); {$endif} // to make assert() below work
    end else begin
      // same offset for all items (fixed sized records) 
      for i := 0 to count-1 do
        PIA^[i+1] := PIA^[i]+diff;
      {$ifopt C+} inc(PI,count+1); count := 0; {$endif} // for assert() below 
    end;
  end else
  repeat
    ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
    while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
      PI^ := FromVarUInt64(P);
      dec(count);
      inc(PI);
    end;
  until count<=0;
  assert((count=0)and(PI=@Values[result]));
end;

function TFileBufferReader.ReadRawUTF8List(List: TRawUTF8List): boolean;
var i: integer;
    StoreObjectsAsVarUInt32: Boolean;
begin
  if (fMap.fBuf<>nil) and (List<>nil) then
  with List do begin
    BeginUpdate;
    try
      Capacity := 0; // finalize both fObjects[] and fList[]
      fCount := ReadVarRawUTF8DynArray(List.fList);
      result := true;
      if fCount=0 then
        exit;
      Read(@StoreObjectsAsVarUInt32,1);
      if StoreObjectsAsVarUInt32 then begin
        SetLength(fObjects,Capacity); 
        for i := 0 to fCount-1 do
          fObjects[i] := TObject(ReadVarUInt32);
      end;
    finally
      EndUpdate;
    end;
  end else
    result := false;
end;

function TFileBufferReader.ReadVarRawUTF8DynArray(var Values: TRawUTF8DynArray): PtrInt;
var count, len, fixedsize: integer;
    P, PEnd: PByte;
    PI: PRawUTF8;
    BufTemp: RawByteString;
begin
  result := ReadVarUInt32;
  if result=0 then
    exit;
  count := result;
  if count>length(Values) then // only set length is not big enough
    SetLength(Values,count);
  PI := pointer(Values);
  fixedsize := ReadVarUInt32;
  repeat
    ReadChunk(P,PEnd,BufTemp); // raise ErrorInvalidContent on error
    if fixedsize=0 then
    while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
      len := FromVarUInt32(P);
      if len>0 then begin
        SetString(PI^,PAnsiChar(P),len);
        inc(P,len);
      end else
        if PI^<>'' then
          PI^ := '';
      dec(count);
      inc(PI);
    end else
    // fixed size strings case
    while (count>0) and (PtrUInt(P)<PtrUInt(PEnd)) do begin
      SetString(PI^,PAnsiChar(P),fixedsize);
      inc(P,fixedsize);
      dec(count);
      inc(PI);
    end;
  until count<=0;
  if PI<>@Values[result] then
    ErrorInvalidContent;
end;

{$ifndef CPU64}
function TFileBufferReader.Seek(Offset: Int64): boolean;
begin
  if (Offset<0) or (Offset>fMap.fFileSize) then
    result := False else
  if fMap.fBuf=nil then
    result := FileSeek64(fMap.fFile,Offset,soFromBeginning)=Offset else begin
    fCurrentPos := Int64Rec(Offset).Lo;
    result := true;
  end;
end;
{$endif CPU64}

function TFileBufferReader.Seek(Offset: PtrInt): boolean;
begin
  // we don't need to handle fMap=0 here
  if fMap.fBuf=nil then
    Result := FileSeek(fMap.fFile,Offset,0)=Offset else
  if (fMap.fBuf<>nil) and (PtrUInt(Offset)<PPtrUInt(@fMap.fFileSize)^) then begin
    fCurrentPos := Offset;
    result := true;
  end else
    result := false;
end;


{ TSynTable }


{$ifndef SORTCOMPAREMETHOD}


function SortU8(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PByte(P1)^-PByte(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU16(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PWord(P1)^-PWord(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PInteger(P1)^-PInteger(P2)^;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortI64(P1,P2: PUTF8Char): PtrInt;
var V: Int64;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PInt64(P1)^-PInt64(P2)^;
        if V<0 then
          result := -1 else
        if V>0 then
          result := 1 else
          result := 0;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortDouble(P1,P2: PUTF8Char): PtrInt;
var V: Double;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        V := PDouble(P1)^-PDouble(P2)^;
        if V<0 then
          result := -1 else
        if V=0 then
          result := 0 else
          result := 1;
       end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortU24(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := PtrInt(PWord(P1)^)+PtrInt(P1[2])shl 16
          -PtrInt(PWord(P2)^)-PtrInt(P2[2]) shl 16;
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarUInt32(PByte(P1))-FromVarUInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt32(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        result := FromVarInt32(PByte(P1))-FromVarInt32(PByte(P2));
        exit;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarUInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then 
        result := FromVarUInt64(PByte(P1))-FromVarUInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortVarInt64(P1,P2: PUTF8Char): PtrInt;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then
        result := FromVarInt64(PByte(P1))-FromVarInt64(PByte(P2)) else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        L := L1;
        if L2>L then
          L := L2;
        for i := 0 to L-1 do begin
          result := PtrInt(P1[i])-PtrInt(P2[i]);
          if Result<>0 then
            exit;
        end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

function SortIStr(P1,P2: PUTF8Char): PtrInt;
var L1, L2, L, i: PtrInt;
    PB1, PB2: PByte;
begin
  if P1<>P2 then
    if P1<>nil then
      if P2<>nil then begin
        if PtrInt(P1^)<=$7F then begin
          L1 := PtrInt(P1^);
          inc(P1);
        end else begin
          PB1 := pointer(P1);
          L1 := FromVarUInt32High(PB1);
          P1 := pointer(PB1);
        end;
        if PtrInt(P2^)<=$7F then begin
          L2 := PtrInt(P2^);
          inc(P2);
        end else begin
          PB2 := pointer(P2);
          L2 := FromVarUInt32High(PB2);
          P2 := pointer(PB2);
        end;
        if L2>L1 then
          L := L2 else
          L := L1;
        for i := 0 to L-1 do // NormToUpperAnsi7 works for both WinAnsi & UTF-8
          if NormToUpperAnsi7[P1[i]]<>NormToUpperAnsi7[P2[i]] then begin
            result := PtrInt(P1[i])-PtrInt(P2[i]);
            exit;
          end;
        result := L1-L2;
      end else
        result := 1 else  // P2=nil
      result := -1 else // P1=nil
    result := 0;      // P1=P2
end;

const
  FIELD_SORT: array[TSynTableFieldType] of TUTF8Compare = (
    nil, // tftUnknown,
    SortU8,    SortU8,  SortU16,  SortU24,  SortI32, SortI64,
 // tftBoolean,tftUInt8,tftUInt16,tftUInt24,tftInt32,tftInt64,
    SortI64,  SortDouble, SortVarUInt32,SortVarInt32,SortVarUInt64,
 // tftCurrency,tftDouble, tftVarUInt32, tftVarInt32,tftVarUInt64,
    SortStr,   SortStr, SortStr,        nil,           SortVarInt64);
 // tftWinAnsi,tftUTF8, tftBlobInternal,tftBlobExternal,tftVarInt64);

{$endif SORTCOMPAREMETHOD}





function GetAddressFromCall(AStub: Pointer): Pointer;
begin
  if AStub=nil then
    result := AStub else
  if PBYTE(AStub)^ = $E8 then begin
    Inc(PtrInt(AStub));
    Result := Pointer(PtrInt(AStub)+SizeOf(integer)+PInteger(AStub)^);
  end else
    Result := nil;
end;



procedure SetReverse(Orig, Reverse: PIntegerArray; Count: integer);
var i: integer;
begin
  i := 0;
  if Count>=4 then begin
    dec(Count,4);
    while i<Count do begin // faster pipelined version
      Reverse[Orig[i]] := i;
      Reverse[Orig[i+1]] := i+1;
      Reverse[Orig[i+2]] := i+2;
      Reverse[Orig[i+3]] := i+3;
      inc(i,4);
    end;
    inc(Count,4);
  end;
  while i<Count do begin
    Reverse[Orig[i]] := i;
    inc(i);
  end;
  //for i := 0 to Count-1 do Assert(Reverse[Orig[i]]=i);
end;

function StrCompL(P1,P2: PUTF8Char; L, Default: Integer): PtrInt;
var i: PtrInt;
begin
  i := 0;
  while i<L do begin
    result := PtrInt(P1[i])-PtrInt(P2[i]);
    if result<>0 then
      exit else
      inc(i);
  end;
  result := Default;
end;

function StrCompIL(P1,P2: PUTF8Char; L, Default: Integer): PtrInt;
var i: PtrInt;
begin
  i := 0;
  while i<L do
  if NormToUpperAnsi7[P1[i]]<>NormToUpperAnsi7[P2[i]] then begin
    result := PtrInt(P1[i])-PtrInt(P2[i]);
    exit;
  end else
    inc(i);
  result := Default;
end;


{ TSynMapFile }

const
  MAGIC_MAB = $A5A5A5A5;

type
  TSynLZHead = packed record
    Magic: cardinal;
    CompressedSize: integer;
    HashCompressed: cardinal;
    UnCompressedSize: integer;
    HashUncompressed: cardinal;
  end;
  PSynLZHead = ^TSynLZHead;
  TSynLZTrailer = packed record
    HeaderRelativeOffset: cardinal;
    Magic: cardinal;
  end;
  PSynLZTrailer = ^TSynLZTrailer;

function StreamSynLZ(Source: TCustomMemoryStream; Dest: TStream; Magic: cardinal): integer;
var DataLen: integer;
    S: pointer;
    P: pointer;
    Head: TSynLZHead;
    Trailer: TSynLZTrailer;
begin
  if Dest=nil then begin
    result := 0;
    exit;
  end;
  if Source<>nil then begin
    S := Source.Memory;
    DataLen := Source.Size;
  end else begin
    S := nil;
    DataLen := 0;
  end;
  Getmem(P,SynLZcompressdestlen(DataLen));
  try
    Head.Magic := Magic;
    Head.UnCompressedSize := DataLen;
    Head.HashUncompressed := Hash32(S,DataLen);
{$ifdef PUREPASCAL}
    result := SynLZcompress1pas(S,DataLen,P); {$else}
    result := SynLZcompress1asm(S,DataLen,P);
{$endif}
    Head.CompressedSize := result;
    Head.HashCompressed := Hash32(P,result);
    Dest.Write(Head,sizeof(Head));
    Dest.Write(P^,Head.CompressedSize);
    Trailer.HeaderRelativeOffset := result+(sizeof(Head)+sizeof(Trailer));
    Trailer.Magic := Magic;
    Dest.Write(Trailer,sizeof(Trailer));
    result := Head.CompressedSize+(sizeof(Head)+sizeof(Trailer));
  finally
    Freemem(P);
  end;
end;

const
  /// 128 MB default buffer
  FILESYNLZ_BLOCKSIZE = 128*1024*1024;
  /// 64 KB overhead seems OK for in-memory compression/decompression
  FILESYNLZ_HEADER = 65536;

function FileSynLZ(const Source, Dest: TFileName; Magic: Cardinal): boolean;
var tmp: RawByteString;
    S,D: TFileStream;
    Head: TSynLZHead;
    P: PAnsiChar;
    Count: Int64;
begin
  result := false;
  if FileExists(Source)  then
  try
    S := TFileStream.Create(Source,fmOpenRead or fmShareDenyNone);
    try
      DeleteFile(Dest);
      D := TFileStream.Create(Dest,fmCreate);
      try
        Head.Magic := Magic;
        Count := S.Size;
        while Count>0 do begin
          if Count>FILESYNLZ_BLOCKSIZE then
            Head.UnCompressedSize := FILESYNLZ_BLOCKSIZE else
            Head.UnCompressedSize := Count;
          if tmp='' then
            SetLength(tmp,Head.UnCompressedSize+FILESYNLZ_HEADER);
          P := pointer(tmp);
          S.Read(P[FILESYNLZ_HEADER],Head.UnCompressedSize);
          Head.HashUncompressed := Hash32(P+FILESYNLZ_HEADER,Head.UnCompressedSize);
{$ifdef PUREPASCAL}
          Head.CompressedSize := SynLZcompress1pas(P+FILESYNLZ_HEADER,Head.UnCompressedSize,P);
{$else}   Head.CompressedSize := SynLZcompress1asm(P+FILESYNLZ_HEADER,Head.UnCompressedSize,P);
{$endif}  Head.HashCompressed := Hash32(P,Head.CompressedSize);
          if (D.Write(Head,sizeof(Head))<>sizeof(Head)) or
             (D.Write(P^,Head.CompressedSize)<>Head.CompressedSize) then
            exit;
          dec(Count,Head.UnCompressedSize);
        end;
        result := FileSetDate(D.Handle,FileGetDate(S.Handle))=0;
      finally
        D.Free;
      end;
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function FileUnSynLZ(const Source, Dest: TFileName; Magic: Cardinal): boolean;
var tmp: RawByteString;
    S,D: TFileStream;
    Count: Int64;
    Head: TSynLZHead;
    PS,PD: PAnsiChar;
begin
  result := false;
  if FileExists(Source)  then
  try
    S := TFileStream.Create(Source,fmOpenRead or fmShareDenyNone);
    try
      DeleteFile(Dest);
      D := TFileStream.Create(Dest,fmCreate);
      try
        Count := S.Size;
        while Count>0 do begin
          if S.Read(Head,sizeof(Head))<>Sizeof(Head) then
            exit;
          dec(Count,sizeof(Head));
          if (Head.Magic<>Magic) or
             (cardinal(Head.UnCompressedSize)>FILESYNLZ_BLOCKSIZE) or
             (Head.CompressedSize>Count) then
            exit;
          if Head.UnCompressedSize+FILESYNLZ_HEADER>length(tmp) then
            SetLength(tmp,Head.UnCompressedSize+FILESYNLZ_HEADER);
          PD := Pointer(tmp);
          PS := PD+length(tmp)-Head.CompressedSize;
          if S.Read(PS^,Head.CompressedSize)<>Head.CompressedSize then
            exit;
          dec(Count,Head.CompressedSize);
          if (Hash32(PS,Head.CompressedSize)<>Head.HashCompressed) or
             (SynLZdecompressdestlen(PS)<>Head.UnCompressedSize) then
            exit;
          {$ifdef PUREPASCAL}
          if (SynLZdecompress1pas(PS,Head.CompressedSize,PD)<>Head.UnCompressedSize) or
          {$else}
          if (SynLZdecompress1asm(PS,Head.CompressedSize,PD)<>Head.UnCompressedSize) or
          {$endif}
             (Hash32(PD,Head.UnCompressedSize)<>Head.HashUncompressed) then
            exit;
          if D.Write(PD^,Head.UncompressedSize)<>Head.UncompressedSize then
            exit;
        end;
        result := FileSetDate(D.Handle,FileGetDate(S.Handle)) =0;
      finally
        D.Free;
      end;
    finally
      S.Free;
    end;
  except
    on Exception do
      result := false;
  end;
end;

function StreamUnSynLZ(const Source: TFileName; Magic: cardinal): TMemoryStream; overload;
var S: TStream;
begin
  try
    S := TSynMemoryStreamMapped.Create(Source);
    try
     result := StreamUnSynLZ(S,Magic);
    finally
      S.Free;
    end;
  except
    on E: Exception do
      result := nil;
  end;
end;

function StreamUnSynLZ(Source: TStream; Magic: cardinal): TMemoryStream;
var P: pointer;
    start, size: Int64;
    Head: TSynLZHead;
    Trailer: TSynLZTrailer;
    buf: RawByteString;
begin
  result := nil;
  if Source=nil then
    exit;
  size := Source.Size;
  start := Source.Position;
  if size-start<sizeof(head) then
    exit;
  Source.Read(Head,sizeof(Head));
  if Head.Magic<>Magic then begin
    // Source not positioned as expected -> try from the end
    Source.Position := size-sizeof(Trailer);
    Source.Read(Trailer,sizeof(Trailer));
    if Trailer.Magic<>Magic then
      exit;
    start := size-Trailer.HeaderRelativeOffset;
    Source.Position := start;
    Source.Read(Head,sizeof(Head));
    if Head.Magic<>Magic then
      exit;
  end;
  inc(start,sizeof(Head));
  if start+Head.CompressedSize>size then
    exit;
  if Source.InheritsFrom(TCustomMemoryStream) then begin
    P := PAnsiChar(TCustomMemoryStream(Source).Memory)+PtrUInt(start);
    Source.Seek(Head.CompressedSize,soFromCurrent);
  end else begin
    SetLength(Buf,Head.CompressedSize);
    P := pointer(Buf);
    Source.Read(P^,Head.CompressedSize);
  end;
  if (Source.Read(Trailer,sizeof(Trailer))=sizeof(Trailer)) and
     (Trailer.Magic<>Magic) then
    exit; // trailer may not be available in old .synlz layout -> ignore
  // Source will now point after all data
  if (SynLZdecompressdestlen(P)<>Head.UnCompressedSize) or
     (Hash32(P,Head.CompressedSize)<>Head.HashCompressed) then
    exit;
  result := THeapMemoryStream.Create;
  result.Size := Head.UnCompressedSize;
  {$ifdef PUREPASCAL}
  if (SynLZdecompress1pas(P,Head.CompressedSize,result.Memory)<>Head.UnCompressedSize) or
  {$else}
  if (SynLZdecompress1asm(P,Head.CompressedSize,result.Memory)<>Head.UnCompressedSize) or
  {$endif}
     (Hash32(result.Memory,Head.UnCompressedSize)<>Head.HashUncompressed) then
    FreeAndNil(result);
end;


function MatchPattern(P,PEnd,Up: PUTF8Char; var Dest: PUTF8Char): boolean;
begin
  result := false;
  repeat
    if P^ in [#1..' '] then repeat inc(P) until not(P^ in [#1..' ']);
    while NormToUpperAnsi7[P^]=Up^ do begin
      inc(P);
      if P>PEnd then exit;
      inc(Up);
      if (Up^=' ') and (P^ in [#1..' ']) then begin // ignore multiple spaces in P^
        while (P<PEnd) and (P^ in [#1..' ']) do inc(P);
        inc(Up);
      end;
    end;
    if Up^=#0 then // all chars of Up^ found in P^
      break else
    if Up^<>' ' then // P^ and Up^ didn't match
      exit;
    inc(Up);
  until false;
  while (P<PEnd) and (P^=' ') do inc(P); // ignore all spaces
  result := true;
  Dest := P;
end;

procedure ReadSymbol(var R: TFileBufferReader; const A: TDynArray);
var i, n, L: integer;
    S: PSynMapSymbol;
    Addr: cardinal;
    P: PByte;
begin
  n := R.ReadVarUInt32;
  A.Count := n;
  P := R.CurrentMemory;
  if (n=0) or (P=nil) then
    exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    L := FromVarUInt32(P); // inlined R.Read(S^.Name)
    SetString(S^.Name,PAnsiChar(P),L);
    inc(P,L);
    inc(PtrUInt(S),A.ElemSize);
  end;
  S := A.Value^;
  Addr := FromVarUInt32(P);
  S^.Start := Addr;
  for i := 1 to n-1 do begin
    inc(Addr,FromVarUInt32(P));
    S^.Stop := Addr-1;
    inc(PtrUInt(S),A.ElemSize);
    S^.Start := Addr;
  end;
  S^.Stop := Addr+FromVarUInt32(P);
  R.fCurrentPos := PtrUInt(P)-PtrUInt(R.fMap.fBuf);
end;

const
  /// Delphi linker starts the code section at this fixed offset
  CODE_SECTION = $1000;


constructor TSynMapFile.Create(const aExeName: TFileName=''; MabCreate: boolean=true);

  procedure LoadMap;
    var P, PEnd: PUTF8Char;
    procedure NextLine;
    begin
      while (P<PEnd) and (P^>=' ') do inc(P);
      if (P<PEnd) and (P^=#13) then inc(P);
      if (P<PEnd) and (P^=#10) then inc(P);
    end;
    function GetCode(var Ptr: cardinal): boolean;
    begin
      while (P<PEnd) and (P^=' ') do inc(P);
      result := false;
      if (P+10<PEnd) and
         (PInteger(P)^=ord('0')+ord('0')shl 8+ord('0')shl 16+ord('1')shl 24) and
         (P[4]=':') then begin
        if not HexDisplayToBin(PAnsiChar(P)+5,@Ptr,sizeof(Ptr)) then exit;
        while (P<PEnd) and (P^>' ') do inc(P);
        while (P<PEnd) and (P^=' ') do inc(P);
        if P<PEnd then
          result := true;
      end;
    end;
    procedure ReadSegments;
    var Beg: PAnsiChar;
        U: TSynMapUnit;
    begin
      NextLine;
      NextLine;
      while (P<PEnd) and (P^<' ') do inc(P);
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(U.Symbol.Start) and
           HexDisplayToBin(PAnsiChar(P),@U.Symbol.Stop,4) then begin
          while PWord(P)^<>ord('M')+ord('=')shl 8 do
            if P+10>PEnd then exit else inc(P);
          Beg := pointer(P+2);
          while (P<PEnd) and (P^>' ') do inc(P);
          SetString(U.Symbol.Name,Beg,P-Beg);
          inc(U.Symbol.Stop,U.Symbol.Start-1);
          if (U.Symbol.Name<>'') and
             ((U.Symbol.Start<>0) or (U.Symbol.Stop<>0)) then
            fUnits.FindHashedAndUpdate(U,true); // true for adding
        end;
        NextLine;
      end;
    end;
    procedure ReadSymbols;
    var Beg: PAnsiChar;
        Sym: TSynMapSymbol;
    begin
      NextLine;
      NextLine;
      while (P+10<PEnd) and (P^>=' ') do begin
        if GetCode(Sym.Start) then begin
          while (P<PEnd) and (P^=' ') do inc(P);
          Beg := pointer(P);
          {$ifdef HASINLINE}
          // trim left 'UnitName.' for each symbol (since Delphi 2005)
          while (P<PEnd) and (P^<>'.') do if P^<=' ' then break else inc(P);
          if P^='.' then begin
            while (P<PEnd) and (P^='.') do inc(P);
            Beg := pointer(P);
          end else
            P := pointer(Beg); // no '.' found
          {$endif}
          while (P<PEnd) and (P^>' ') do inc(P);
          SetString(Sym.Name,Beg,P-Beg);
          if Sym.Name<>'' then
            fSymbols.Add(Sym);
        end;
        NextLine;
      end;
    end;
    procedure ReadLines;
    var Beg: PAnsiChar;
        i, Count, n: integer;
        aName: RawUTF8;
        added: boolean;
        U: ^TSynMapUnit;
    begin
      Beg := pointer(P);
      while P^<>'(' do if P=PEnd then exit else inc(P);
      SetString(aName,Beg,P-Beg);
      if aName='' then
        exit;
      i := fUnits.FindHashedForAdding(aName,added);
      U := @fUnit[i];
      if added then
        U^.Symbol.Name := aName; // should not occur, but who knows...
      if U^.FileName='' then begin
        inc(P); Beg := pointer(P);
        while P^<>')' do if P=PEnd then exit else inc(P);
        SetString(U^.FileName,Beg,P-Beg);
      end;
      NextLine;
      NextLine;
      n := length(U^.Line);
      Count := n; // same unit may appear multiple times in .map content
      while (P+10<PEnd) and (P^>=' ') do begin
        while (P<PEnd) and (P^=' ') do inc(P);
        repeat
          if Count=n then begin
            n := Count+256;
            SetLength(U^.Line,n);
            SetLength(U^.Addr,n);
          end;
          U^.Line[Count] := GetNextItemCardinal(P,' ');
          if not GetCode(cardinal(U^.Addr[Count])) then
            break;
          if U^.Addr[Count]<>0 then
            inc(Count); // occured with Delphi 2010 :(
        until (P>=PEnd) or (P^<' ');
        NextLine;
      end;
      SetLength(U^.Line,Count);
      SetLength(U^.Addr,Count);
    end;
  var i, s,u: integer;
  begin // LoadMap
    fSymbols.Capacity := 8000;
    with TSynMemoryStreamMapped.Create(fMapFile) do
    try
      // parse .map sections into fSymbol[] and fUnit[]
      P := Memory;
      PEnd := P+Size;
      while P<PEnd do
        if MatchPattern(P,PEnd,'DETAILED MAP OF SEGMENTS',P) then
          ReadSegments else
        if MatchPattern(P,PEnd,'ADDRESS PUBLICS BY VALUE',P) then
          ReadSymbols else
        if MatchPattern(P,PEnd,'LINE NUMBERS FOR',P) then
          ReadLines else
         NextLine;
      // now we should have read all .map content
      s := fSymbols.Count-1;
      for i := fUnits.Count-1 downto 0 do
        with fUnit[i] do
          if (Symbol.Start=0) and (Symbol.Stop=0) then
            fUnits.Delete(i); // occured with Delphi 2010 :(
      u := fUnits.Count-1;
      {$ifopt C+}
      for i := 1 to u do
         assert(fUnit[i].Symbol.Start>fUnit[i-1].Symbol.Stop);
      {$endif}
      for i := 0 to s-1 do
        fSymbol[i].Stop := fSymbol[i+1].Start-1;
      if (u>=0) and (s>=0) then
        fSymbol[s].Stop := fUnit[u].Symbol.Stop;
    finally
      Free;
    end;
  end;

  procedure LoadMab(const aMabFile: TFileName);
  var R: TFileBufferReader;
      i: integer;
      S: TCustomMemoryStream;
      MS: TMemoryStream;
  begin
    fMapFile := aMabFile;
    if FileExists(aMabfile) then
    try
      S := TSynMemoryStreamMapped.Create(aMabFile);
      try
        MS := StreamUnSynLZ(S,MAGIC_MAB);
        if MS<>nil then
        try
          R.OpenFrom(MS.Memory,MS.Size);
          ReadSymbol(R,fSymbols);
          ReadSymbol(R,fUnits);
          for i := 0 to fUnits.Count-1 do
          with fUnit[i] do begin
            R.Read(FileName);
            R.ReadVarUInt32Array(Line);
            R.ReadVarUInt32Array(Addr);
          end;
          MabCreate := false;
        finally
          MS.Free;
        end;
      finally
        S.Free;
      end;
    except
      on Exception do; // invalid file -> ignore any problem
    end;
  end;

var SymCount, UnitCount, i: integer;
    MabFile: TFileName;
    MapAge, MabAge: TDateTime;
begin
  fSymbols.Init(TypeInfo(TSynMapSymbolDynArray),fSymbol,@SymCount);
  fUnits.Init(TypeInfo(TSynMapUnitDynArray),fUnit,nil,nil,nil,@UnitCount);
  // 1. search for an external .map file matching the running .exe/.dll name
  if aExeName='' then begin
    fMapFile := GetModuleName(hInstance);
    fGetModuleHandle := GetModuleHandle(pointer(ExtractFileName(fMapFile)))+CODE_SECTION;
  end else
    fMapFile := aExeName;
  fMapFile := ChangeFileExt(fMapFile,'.map');
  MabFile := ChangeFileExt(fMapFile,'.mab');
  MapAge := FileAgeToDateTime(fMapFile);
  MabAge := FileAgeToDateTime(MabFile);
  if (MabAge<=MapAge) and (MapAge>0) then
    LoadMap; // if no faster-to-load .mab available and accurate
  // 2. search for a .mab file matching the running .exe/.dll name
  if (SymCount=0) and (MabAge<>0) then
    LoadMab(MabFile);
  // 3. search for an embedded compressed .mab file appended to the .exe/.dll
  if SymCount=0 then
    LoadMab(GetModuleName(hInstance));
  // finalize symbols
  if SymCount>0 then begin
    for i := 1 to SymCount-1 do
      assert(fSymbol[i].Start>fSymbol[i-1].Stop);
    SetLength(fSymbol,SymCount);
    SetLength(fUnit,UnitCount);
    fSymbols.fCountP := nil;
    fUnits.fCountP := nil;
    if MabCreate then
      SaveToFile(MabFile); // if just created from .map -> create .mab file 
    fHasDebugInfo := true;
  end else
    fMapFile := '';
end;

procedure WriteSymbol(var W: TFileBufferWriter; const A: TDynArray);
var i, n: integer;
    Diff: cardinal;
    S: PSynMapSymbol;
    P: PByte;
    Beg: PtrUInt;
begin
  n := A.Count;
  W.WriteVarUInt32(n);
  if n=0 then exit;
  S := A.Value^;
  for i := 0 to n-1 do begin
    W.Write(S^.Name);
    inc(PtrUInt(S),A.ElemSize);
  end;
  S := A.Value^;
  Diff := S^.Start;
  W.WriteVarUInt32(Diff);
  if W.fPos+n*5>W.fBufLen then
    W.fTotalWritten := W.Flush;
  with W do
    if fPos+n*5>fBufLen then // BufLen=1 shl 19=512 KB should be enough
      raise ESynException.CreateFmt('too big %s',[
        PShortString(PDynArrayTypeInfo(A.TypeInfo).NameLen)^]) else
      P := @PByteArray(fBuf)^[fPos];
  Beg := PtrUInt(P);
  for i := 1 to n-1 do begin
    inc(PtrUInt(S),A.ElemSize);
    P := ToVarUInt32(S^.Start-Diff,P);
    Diff := S^.Start;
  end;
  P := ToVarUInt32(S^.Stop-Diff,P);
  Beg := PtrUInt(P)-Beg;
  inc(W.fPos,Beg);
  inc(W.fTotalWritten,Beg);
end;

procedure TSynMapFile.SaveToStream(aStream: TStream);
var W: TFileBufferWriter;
    i: integer;
    MS: TMemoryStream;
begin
  MS := THeapMemoryStream.Create;
  W := TFileBufferWriter.Create(MS,1 shl 20); // 1 MB should be enough
  try
    WriteSymbol(W,fSymbols);
    WriteSymbol(W,fUnits);
    for i := 0 to high(fUnit) do
    with fUnit[i] do begin
      W.Write(FileName);
      W.WriteVarUInt32Array(Line,length(Line),wkOffsetI); // not always increasing
      W.WriteVarUInt32Array(Addr,length(Addr),wkOffsetU); // always increasing
    end;
    W.Flush;
    StreamSynLZ(MS,aStream,MAGIC_MAB);
  finally
    MS.Free;
    W.Free;
  end;
end;

function TSynMapFile.SaveToFile(const aFileName: TFileName=''): TFileName;
var F: TFileStream;
begin
  if aFileName='' then
    result := ChangeFileExt(GetModuleName(hInstance),'.mab') else
    result := aFileName;
  DeleteFile(result);
  F := TFileStream.Create(result,fmCreate);
  try
    SaveToStream(F);
  finally
    F.Free;
  end;
end;

procedure TSynMapFile.SaveToExe(const aExeName: TFileName);
var FN: TFileName;
    MS, MAB: TMemoryStream;
    Len, LenMAB: PtrUInt;
    P: PAnsiChar;
begin
  if not FileExists(aExeName) then
    exit;
  FN := SaveToFile(ChangeFileExt(aExeName,'.mab'));
  try
    MS := THeapMemoryStream.Create;
    MAB := THeapMemoryStream.Create;
    try
      // load both files
      MAB.LoadFromFile(FN);
      LenMAB := MAB.Size;
      MS.LoadFromFile(aExeName);
      Len := MS.Size;
      if Len<16 then
        exit;
      P := MS.Memory;
      inc(P,Len);
      with PSynLZTrailer(P-sizeof(TSynLZTrailer))^ do
        if (Magic=MAGIC_MAB) and (HeaderRelativeOffset<Len) and
           (PSynLZHead(P-HeaderRelativeOffset)^.Magic=MAGIC_MAB) then
          // trim existing mab content
          dec(Len,HeaderRelativeOffset);
      // append mab content to exe
      MS.Size := Len+LenMAB;
      move(MAB.Memory^,PAnsiChar(MS.Memory)[Len],LenMAB);
      MS.SaveToFile(aExeName);
    finally
      MAB.Free;
      MS.Free;
    end;
  finally
    DeleteFile(FN);
  end;
end;

function TSynMapFile.FindSymbol(aAddr: cardinal): integer;
var L,R: integer;
begin
  R := high(fSymbol);
  L := 0;
  if (R>=0) and (aAddr>=fSymbol[0].Start) and (aAddr<=fSymbol[R].Stop) then
  repeat
    result := (L+R)shr 1;
    with fSymbol[result] do
      if aAddr<Start then
        R := result-1 else
      if aAddr>Stop then
        L := result+1 else
        exit;
  until L>R;
  result := -1;
end;

function TSynMapFile.FindUnit(aAddr: cardinal; out LineNumber: integer): integer;
var L,R,n,max: integer;
begin
  LineNumber := 0;
  R := high(fUnit);
  L := 0;
  if (R>=0) and
     (aAddr>=fUnit[0].Symbol.Start) and (aAddr<=fUnit[R].Symbol.Stop) then
  repeat
    result := (L+R) shr 1;
    with fUnit[result] do
      if aAddr<Symbol.Start then
        R := result-1 else
      if aAddr>Symbol.Stop then
        L := result+1 else begin
        // unit found -> search line number
        L := 0;
        max := high(Addr);
        R := max;
        if R>=0 then
        repeat
          n := (L+R) shr 1;
          if aAddr<cardinal(Addr[n]) then
            R := n-1 else
          if (n<max) and (aAddr>=cardinal(Addr[n+1])) then
            L := n+1 else begin
            LineNumber := Line[n];
            exit;
          end;
        until L>R;
        exit;
      end;
  until L>R;
  result := -1;
end;

var
  InstanceMapFile: TSynMapFile;
  
class procedure TSynMapFile.Log(W: TTextWriter; Addr: PtrUInt);
var u, s, Line: integer;
begin
  if (W=nil) or (Addr=0) or (InstanceMapFile=nil) then
    exit;
  with InstanceMapFile do
  if HasDebugInfo then begin
    dec(Addr,fGetModuleHandle);
    s := FindSymbol(Addr);
    u := FindUnit(Addr,Line);
    if s<0 then begin
      if u<0 then
        exit;
    end else
      with PInt64Rec(Symbols[s].Name)^ do
      if Lo=ord('T')+ord('S')shl 8+ord('y')shl 16+ord('n')shl 24 then
      case Hi of
        ord('L')+ord('o')shl 8+ord('g')shl 16+ord('.')shl 24,
        ord('T')+ord('e')shl 8+ord('s')shl 16+ord('t')shl 24:
          exit; // don't log stack trace internal to TSynLog.*/TSynTest* methods
      end;
    W.AddPointer(Addr); // only display addresses inside known Delphi code
    W.Add(' ');
    if u>=0 then begin
      W.AddString(Units[u].Symbol.Name);
      if s>=0 then
        if Symbols[s].Name=Units[u].Symbol.Name then
          s := -1 else
          W.Add('.');
    end;
    if s>=0 then
      W.AddString(Symbols[s].Name);
    W.Add(' ');
    if Line>0 then begin
      W.Add('(');
      W.Add(Line);
      W.Add(')',' ');
    end;
  end else begin
    W.AddPointer(Addr); // no .map info available -> display all addresses 
    W.Add(' ');
  end;
end;

function TSynMapFile.FindLocation(aAddr: Cardinal): RawUTF8;
var u,s,Line: integer;
begin
  result := '';
  if (aAddr=0) or not HasDebugInfo then
    exit;
  dec(aAddr,fGetModuleHandle);
  s := FindSymbol(aAddr);
  u := FindUnit(aAddr,Line);
  if (s<0) and (u<0) then
    exit;
  if u>=0 then begin
    with Units[u] do begin
      if FileName<>'' then
        result := FileName+' - ';
      result := result+Symbol.Name;
    end;
    if s>=0 then
      if Symbols[s].Name=Units[u].Symbol.Name then
        s := -1 else
        result := result+'.';
  end;
  if s>=0 then
    result := result+Symbols[s].Name;
  if Line>0 then
    result := result+' ('+Int32ToUtf8(Line)+')';
end;


{ TSynLogFamily }

var
  /// internal list of registered TSynLogFamily
  // - up to MAX_SYNLOGFAMILY+1 families may be defined
  SynLogFamilys: TObjectList = nil;


{$ifndef NOEXCEPTIONINTERCEPT}

/// if defined, will use AddVectoredExceptionHandler() API call
// - this one does not produce accurate stack trace by now, and is supported
// only since Windows XP
// - so default method using RTLUnwindProc should be prefered
{.$define WITH_VECTOREXCEPT}

{$ifdef DELPHI5OROLDER}
// Delphi 5 doesn't define the needed RTLUnwindProc variable :(
// so we will patch the System.pas RTL in-place

var
  RTLUnwindProc: Pointer;

procedure PatchCallRtlUnWind;
procedure Patch(P: PAnsiChar);
{   004038B6 52               push edx  // Save exception object
    004038B7 51               push ecx  // Save exception address
    004038B8 8B542428         mov edx,[esp+$28]
    004038BC 83480402         or dword ptr [eax+$04],$02
    004038C0 56               push esi  // Save handler entry
    004038C1 6A00             push $00
    004038C3 50               push eax
    004038C4 68CF384000       push $004038cf  // @@returnAddress
    004038C9 52               push edx
    004038CA E88DD8FFFF       call RtlUnwind
    ...
RtlUnwind:
    0040115C FF255CC14100     jmp dword ptr [$0041c15c]
    where $0041c15c is a pointer to the address of RtlUnWind in kernel32.dll
    -> we will replace [$0041c15c] by [RTLUnwindProc]    }
var i: Integer;
    addr: PAnsiChar;
begin
  for i := 0 to 31 do
    if (PCardinal(P)^=$6850006a) and  // push 0; push eax; push @@returnAddress
       (PWord(P+8)^=$E852) then begin // push edx; call RtlUnwind
      inc(P,10); // go to call RtlUnwind address
      if PInteger(P)^<0 then begin
        addr := P+4+PInteger(P)^;
        if PWord(addr)^=$25FF then begin // jmp dword ptr []
          PatchCodePtrUInt(Pointer(addr+2),cardinal(@RTLUnwindProc));
          exit;
        end;
      end;
    end else
    inc(P);
end;
asm
  mov eax,offset System.@HandleAnyException+200
  call Patch
end;
{$endif}

var
  SynLogExceptionEnabled: boolean = false;

type
  /// an array to all available per-thread TSynLogFile instances
  TSynLogFileIndex = array[0..MAX_SYNLOGFAMILY] of integer;

var
  /// internal list of created TSynLog instance, one per each log file on disk
  // - do not use directly - necessary for inlining TSynLogFamily.SynLog method
  SynLogFiles: TObjectList = nil;

threadvar
  /// each thread can access to its own TSynLogFile
  // - TSynLogFile instance is SynLogFile[SynLogFileIndex[TSynLogFamily.Ident]-1]
  SynLogFileIndex: TSynLogFileIndex;
  /// used internaly by function GetHandleExceptionSynLog
  
  CurrentHandleExceptionSynLog: TSynLog;


function GetHandleExceptionSynLog: TSynLog;
var Index: ^TSynLogFileIndex;
    i: integer;
    ndx, n: cardinal;
begin
  result := CurrentHandleExceptionSynLog;
  if (result<>nil) and result.fFamily.fHandleExceptions then
    exit;
  Index := @SynLogFileIndex;
  if SynLogFiles=nil then
  begin
    // we are here is no previous log content was trigerred
    for i := 0 to SynLogFamilys.Count-1 do
      with TSynLogFamily(SynLogFamilys.List[i]) do
      if fHandleExceptions then
      begin
        result := SynLog;
        exit;
      end;
  end else
  begin
    n := SynLogFiles.Count;
    for i := 0 to high(Index^) do begin
      ndx := Index^[i]-1;
      if ndx<=n then begin
        result := TSynLog(SynLogFiles.List[ndx]);
        if result.fFamily.fHandleExceptions then
          exit;
      end;
    end;
    for i := 0 to n-1 do begin
      result := TSynLog(SynLogFiles.List[i]);
      if result.fFamily.fHandleExceptions then
        exit;
    end;
  end;
  result := nil;
end;

type
  PExceptionRecord = ^TExceptionRecord;
  TExceptionRecord = record
    ExceptionCode: DWord;
    ExceptionFlags: DWord;
    OuterException: PExceptionRecord;
    ExceptionAddress: PtrUInt;
    NumberParameters: Longint;
    case {IsOsException:} Boolean of
    True:  (ExceptionInformation : array [0..14] of PtrUInt);
    False: (ExceptAddr: PtrUInt; ExceptObject: Exception);
  end;
  GetExceptionClass = function(const P: TExceptionRecord): ExceptClass;

const
  cDelphiExcept = $0EEDFAE0;
  cDelphiException = $0EEDFADE;

procedure LogExcept(stack: PPtrUInt; const Exc: TExceptionRecord);
var SynLog: TSynLog;
    Level: TSynLogInfo;
    E: ExceptClass;
    LastError: DWORD;
    EAddr: PtrUInt;
begin
  SynLog := GetHandleExceptionSynLog;
  if SynLog=nil then
    exit;
  LastError := GetLastError;
  if (Exc.ExceptionCode=cDelphiException) and (Exc.ExceptObject<>nil) then
  begin
    if Exc.ExceptObject.InheritsFrom(Exception) then
      E := PPointer(Exc.ExceptObject)^ else
      E := EExternalException;
    if sllException in SynLog.fFamily.Level then
      Level := sllException else
      Level := sllError;
    EAddr := Exc.ExceptAddr;
  end else begin
    if Assigned(ExceptClsProc) then
      E := GetExceptionClass(ExceptClsProc)(Exc) else
      E := EExternal;
    Level := sllExceptionOS;
    EAddr := Exc.ExceptionAddress;
  end;
  if (Level<>sllError) and
     (SynLog.fFamily.ExceptionIgnore.IndexOf(E)<0) then
  begin
    SynLog.Lock;
    try
      SynLog.LogHeader(Level);
      with SynLog.fWriter do begin
        AddClassName(E);
        if (Level=sllException) and (E<>EExternalException) then begin
          AddShort(' ("');
          AddJSONEscapeString(Exc.ExceptObject.Message);
          AddShort('") at ');
        end else begin
          AddShort(' (');
          AddPointer(Exc.ExceptionCode);
          AddShort(') at ');
        end;
      end;
      TSynMapFile.Log(SynLog.fWriter,EAddr);
  {$ifndef WITH_VECTOREXCEPT} // stack frame is only correct for RTLUnwindProc
      SynLog.AddStackTrace(stack);
  {$endif}
      SynLog.fWriter.AddCR;
      SynLog.fWriter.Flush; // we expect exceptions to be available on disk
    finally
      SynLog.Unlock;
    end;
  end;
  SetLastError(LastError); // code above could have changed this
end;


{$ifdef WITH_VECTOREXCEPT}
type
  PExceptionInfo = ^TExceptionInfo;
  TExceptionInfo = packed record
    ExceptionRecord: PExceptionRecord;
    ContextRecord: pointer;
  end;

var
  AddVectoredExceptionHandler: function(FirstHandler: cardinal;
    VectoredHandler: pointer): PtrInt; stdcall;

function SynLogVectoredHandler(ExceptionInfo : PExceptionInfo): PtrInt; stdcall;
const
  EXCEPTION_CONTINUE_SEARCH = 0;
begin
  if SynLogExceptionEnabled then
    LogExcept(nil,ExceptionInfo^.ExceptionRecord^);
  result := EXCEPTION_CONTINUE_SEARCH;
end;

{$else WITH_VECTOREXCEPT}

{$ifdef DELPHI5OROLDER}
procedure RtlUnwind; external kernel32 name 'RtlUnwind';
{$else}
var
  oldUnWindProc: pointer;
{$endif}

procedure SynRtlUnwind(TargetFrame, TargetIp: pointer;
  ExceptionRecord: PExceptionRecord; ReturnValue: Pointer); stdcall;
asm
  pushad
  cmp  byte ptr SynLogExceptionEnabled,0
  jz   @oldproc
  mov  eax,TargetFrame
  mov  edx,ExceptionRecord
  call LogExcept
@oldproc:
  popad
  pop ebp // hidden push ebp at asm level
{$ifdef DELPHI5OROLDER}
  jmp RtlUnwind
{$else}
  jmp oldUnWindProc
{$endif}
end;

{$endif WITH_VECTOREXCEPT}

{$endif NOEXCEPTIONINTERCEPT}

procedure TSynLogFamily.SetLevel(aLevel: TSynLogInfos);
begin
  // ensure BOTH Enter+Leave are always selected at once, if any is set
  if sllEnter in aLevel then
    include(aLevel,sllLeave)
  else
  if sllLeave in aLevel then
    include(aLevel,sllEnter);
  fLevel := aLevel;
{$ifndef NOEXCEPTIONINTERCEPT}
  // intercept exceptions, if necessary
  fHandleExceptions := (sllExceptionOS in aLevel) or (sllException in aLevel);
  if fHandleExceptions and not SynLogExceptionEnabled then
  begin
    SynLogExceptionEnabled := true;
  {$ifdef WITH_VECTOREXCEPT}
    AddVectoredExceptionHandler :=
      GetProcAddress(GetModuleHandle(kernel32),'AddVectoredExceptionHandler');
    // RemoveVectoredContinueHandler() is available under 64 bit editions only
    if Assigned(AddVectoredExceptionHandler) then
      // available since Windows XP
      AddVectoredExceptionHandler(0,@SynLogVectoredHandler);
  {$else WITH_VECTOREXCEPT}
  {$ifdef DELPHI5OROLDER}
    PatchCallRtlUnWind;
  {$else}
    oldUnWindProc := RTLUnwindProc;
  {$endif}
    RTLUnwindProc := @SynRtlUnwind;
  {$endif WITH_VECTOREXCEPT}
  end;
{$endif NOEXCEPTIONINTERCEPT}
end;

constructor TSynLogFamily.Create{(aSynLog: TSynLogClass)};
begin
 // fSynLogClass := aSynLog;
  if SynLogFamilys=nil then
  begin
    SynLogFamilys := TObjectList.Create;
    GarbageCollector.Add(SynLogFamilys);
  end;
  fIdent := SynLogFamilys.Add(self);
  fDestinationPath := ExtractFilePath(paramstr(0)); // use .exe path 
  fDefaultExtension := '.log';
  fArchivePath := fDestinationPath;
  fArchiveAfterDays := 7;
  fBufferSize := 4096;
  fStackTraceLevel := 20;
  fMaxMBSize := 10;
  fExceptionIgnore := TList.Create;
  fLevelStackTrace :=
    [sllException,sllExceptionOS,sllFail,sllLastError,sllStackTrace];
end;

function TSynLogFamily.CreateSynLog: TSynLog;
var i: integer;
begin
  if SynLogFiles=nil then
  begin
    SynLogFiles := TObjectList.Create;
    GarbageCollector.Add(SynLogFiles);
  end;
  result := TSynLog.Create(self);
  i := SynLogFiles.Add(result);
  //if fPerThreadLog then
  SynLogFileIndex[fIdent] := i+1;
  {else
    fGlobalLog := result;  }
end;

var
  AutoFlushThread: THandle = 0;
  AutoFlushSecondElapsed: cardinal;

procedure AutoFlushProc(P: pointer); stdcall;  // TThread not needed here
var i: integer;
begin
  repeat
    Sleep(1000); // thread will awake every second to check of pending data
    if AutoFlushThread=0 then
      break; // avoid GPF
    if SynLogFiles=nil then
      continue; // nothing to flush
    inc(AutoFlushSecondElapsed);
    for i := 0 to SynLogFiles.Count-1 do
    with TSynLog(SynLogFiles.List[i]) do
      if AutoFlushThread=0 then
        break else // avoid GPF
      if (fFamily.fAutoFlush<>0) and (fWriter<>nil) and
         (AutoFlushSecondElapsed mod fFamily.fAutoFlush=0) then
        if fWriter.B-fWriter.fTempBuf>1 then begin
          if not IsMultiThread and
             not fWriterStream.InheritsFrom(TFileStream) then
            IsMultiThread := true; // only TFileStream is thread-safe
          Flush(false); // write pending data
        end;
  until false;
  ExitThread(0);
end;

procedure TSynLogFamily.SetAutoFlush(TimeOut: cardinal);
var ID: cardinal;
begin
  SynLog.CreateLocker;
  fAutoFlush := TimeOut;
  if (AutoFlushThread=0) and (TimeOut<>0) and (DebugHook=0) then
  begin
    AutoFlushSecondElapsed := 0;
    AutoFlushThread := CreateThread(nil,0,@AutoFlushProc,nil,0,ID);
  end;
end;

destructor TSynLogFamily.Destroy;
var SR: TSearchRec;
    OldTime: integer;
    aTime: TDateTime;
    Y,M,D: word;
    aOldLogFileName, aPath: TFileName;
    //tmp: array[0..11] of AnsiChar;
    tmp: array[0..7] of AnsiChar;
begin
  if AutoFlushThread<>0 then begin
    CloseHandle(AutoFlushThread); // release background thread once for all
    AutoFlushThread := 0; // mark thread released to avoid GPF in AutoFlushProc
  end;
  ExceptionIgnore.Free;
  try
    if assigned(BeforeDestroy) then
    BeforeDestroy(self);
    if Assigned(OnArchive) then
    if FindFirst(DestinationPath+'*'+DefaultExtension,faAnyFile,SR)=0 then
    try
      if ArchiveAfterDays<0 then
        ArchiveAfterDays := 0;
      OldTime := DateTimeToFileDate(Now-ArchiveAfterDays);
      repeat
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED OFF} // for SR.Time
        {$endif}
        if (SR.Name[1]='.') or (faDirectory and SR.Attr<>0) or
           (SR.Time>OldTime) then
          continue;
        {$ifndef DELPHI5OROLDER}
        {$WARN SYMBOL_DEPRECATED ON}
        {$endif}
        aOldLogFileName := DestinationPath+SR.Name;
        if aPath='' then begin
          aTime := FileAgeToDateTime(aOldLogFileName);
          if (aTime=0) {or
             not DirectoryExists(ArchivePath) and
             not CreateDir(ArchivePath)
            { not DirectoryExists(ArchivePath) and
             not CreateDir(ArchivePath)  }
             then break;
          DecodeDate(aTime,Y,M,D);
          //PCardinal(@tmp)^ := ord('l')+ord('o') shl 8+ord('g') shl 16+ord('\') shl 24;
          tmp[0] := '\';//+ord('o') shl 8+ord('g') shl 16+ord('\') shl 24;
          //YearToPChar(Y,@tmp[4]);
          YearToPChar(Y,@tmp[1]);
         { PWord(@tmp[8])^ := TwoDigitLookupW[M];
          PWord(@tmp[10])^ := ord('\');}
          PWord(@tmp[5])^ := TwoDigitLookupW[M];
         // PWord(@tmp[7])^ := ord('\');
          //aPath := ArchivePath+Ansi7ToString(tmp,11);
          aPath := ArchivePath+Ansi7ToString(tmp,7);
        end;
        OnArchive(aOldLogFileName,aPath);
      until FindNext(SR) <>0;
    finally
      try
        OnArchive('',aPath); // mark no more .log file to archive -> close .zip
      finally
        FindClose(SR);
      end;
    end;
  finally
    inherited;
  end;
end;

function TSynLogFamily.SynLog: TSynLog;
var ndx: integer;
begin
  ndx := SynLogFileIndex[fIdent]-1;
  if ndx>=0 then
    result := SynLogFiles.List[ndx]
  else
    result := CreateSynLog;
{$ifndef NOEXCEPTIONINTERCEPT}
  if fHandleExceptions then
    CurrentHandleExceptionSynLog := result;
{$endif}
end;



{ TSynLog }

var
  RtlCaptureStackBackTraceRetrieved: (btUntested, btOK, btFailed) = btUntested;
  RtlCaptureStackBackTrace: function(FramesToSkip, FramesToCapture: cardinal;
    BackTrace, BackTraceHash: pointer): byte; stdcall;

constructor TSynLog.Create(aFamily: TSynLogFamily);
begin
  CreateLocker;
  fFamily := aFamily;
  if RtlCaptureStackBackTraceRetrieved=btUntested then
  begin
    if OSVersion<wXP then
      RtlCaptureStackBackTraceRetrieved := btFailed
    else
    begin
      @RtlCaptureStackBackTrace := GetProcAddress(GetModuleHandle(kernel32),'RtlCaptureStackBackTrace');
      if @RtlCaptureStackBackTrace=nil then
        RtlCaptureStackBackTraceRetrieved := btFailed
      else
       RtlCaptureStackBackTraceRetrieved := btOK;
    end;
  end;
  {$ifdef CPU64}
  assert(RtlCaptureStackBackTraceRetrieved=btOK);
  {$endif}
end;

destructor TSynLog.Destroy;
begin
  Flush(true);
  fWriterStream.Free;
  fWriter.Free;
  if fLockCS<>nil then
  begin
    DeleteCriticalSection(fLockCS^);
    FreeMem(fLockCS);
  end;
  FreeMem(fRecursion);
  inherited;
end;           

procedure TSynLog.Flush(ForceDiskWrite: boolean);
begin
  if fWriter=nil then
    exit;
  Lock;
  try
    fWriter.Flush;
    if ForceDiskWrite and fWriterStream.InheritsFrom(TFileStream) then
      FlushFileBuffers(TFileStream(fWriterStream).Handle);
  finally
    UnLock;
  end;
end;

procedure TSynLog.RecursionGrow;
begin
  inc(fRecursionMax,256+fRecursionMax shr 3);
  ReallocMem(fRecursion,fRecursionMax*sizeof(fRecursion^[0]));
end;

procedure TSynLog.Enter(aClassType: TClass; aMethodName: PUTF8Char);
var
    aStackFrame: PtrUInt;
begin
  if sllEnter in fFamily.fLevel then
  begin
    // recursively store parameters
    if fRecursionCount=fRecursionMax then
      RecursionGrow;
    {$ifdef CPU64}
    if RtlCaptureStackBackTrace(1,1,@aStackFrame,nil)=0 then
      aStackFrame := 0
    else
      dec(aStackFrame,5); // ignore call TSynLog.Enter op codes
    {$else}
    asm
      mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
      sub eax,5        // ignore call TSynLog.Enter op codes
      mov aStackFrame,eax
    end;
    {$endif}
    with fRecursion^[fRecursionCount] do
    begin
      Instance := nil;
      ClassType := aClassType;
      Method := aMethodName;
      Caller := aStackFrame;
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
 // result := aSynLog;
end;

procedure TSynLog.Enter(aInstance: TObject; aMethodName: PUTF8Char);
var
aStackFrame: PtrUInt;
begin
  if sllEnter in fFamily.fLevel then
  begin
    if fRecursionCount=fRecursionMax then
      RecursionGrow;
    {$ifdef CPU64}
    if RtlCaptureStackBackTrace(1,1,@aStackFrame,nil)=0 then
      aStackFrame := 0
    else
      dec(aStackFrame,5); // ignore call TSynLog.Enter op codes
    {$else}
    asm
      mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
      sub eax,5        // ignore call TSynLog.Enter op codes
      mov aStackFrame,eax
    end;
    {$endif}
    with fRecursion^[fRecursionCount] do
    begin
      Instance := aInstance;
      if aInstance=nil then
        ClassType := pointer(aInstance) else
        ClassType := PPointer(aInstance)^;
      Method := aMethodName;
      Caller := aStackFrame;
    end;
  end;
  // copy to ISynLog interface -> will call TSynLog._AddRef
  //result := aSynLog;
end;

{$ifndef DELPHI5OROLDER}
procedure TSynLog.Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArgs: array of const;
  aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,TextArgs,aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArg: RawUTF8;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; TextFmt: PWinAnsiChar; const TextArg: Int64;
  aInstance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,TextFmt,[TextArg],aInstance);
end;
{$endif}

procedure TSynLog.Log(Level: TSynLogInfo; const Text: RawUTF8; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,Text,aInstance);
end;

procedure TSynLog.Log(Level: TSynLogInfo; aInstance: TObject);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    if aInstance<>nil then
      LogInternal(Level,'',aInstance) else
      LogInternal(Level,'Instance=nil',nil);
end;

procedure TSynLog.Log(Level: TSynLogInfo; aName: PWinAnsiChar;
  aTypeInfo: pointer; var aValue; Instance: TObject=nil);
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
    LogInternal(Level,aName,aTypeInfo,aValue,Instance);
end;

procedure TSynLog.Log(Level: TSynLogInfo);
var aCaller: PtrUInt;
begin
  if (self<>nil) and (Level in fFamily.fLevel) then
  begin
    Lock;
    try
      LogHeader(Level);
      {$ifdef CPU64}
      if RtlCaptureStackBackTrace(1,1,@aCaller,nil)=0 then
        aCaller := 0 else
        dec(aCaller,5); // ignore call TSynLog.Enter op codes
      {$else}
      asm
        mov eax,[ebp+4]  // retrieve caller EIP from push ebp; mov ebp,esp
        sub eax,5        // ignore call TSynLog.Enter op codes
        mov aCaller,eax
      end;
      {$endif}
      TSynMapFile.Log(fWriter,aCaller); 
      LogTrailer(Level);
    finally
      Unlock;
    end;
  end;
end;

procedure TSynLog.LogFileHeader;
begin
  if not QueryPerformanceFrequency(fFrequencyTimeStamp) then begin
    fFamily.HighResolutionTimeStamp := false;
    fFrequencyTimeStamp := 0;
  end;
  ExeVersionRetrieve;
  if InstanceMapFile=nil then begin
    InstanceMapFile := TSynMapFile.Create;
    GarbageCollector.Add(InstanceMapFile);
  end;
  // array of const is buggy under Delphi 5 :( -> use fWriter.Add*()
  with ExeVersion, SystemInfo, OSVersionInfo, fWriter do begin
    AddString(ProgramFullSpec);
    AddShort(#13'Host='); AddString(Host);
    AddShort(' User=');   AddString(User);
    AddShort(' CPU='); Add(dwNumberOfProcessors); Add('*');
    Add(wProcessorArchitecture); Add('-'); Add(wProcessorLevel); Add('-');
    Add(wProcessorRevision);
    AddShort(' OS='); Add(ord(OSVersion)); Add('.'); Add(wServicePackMajor);
    Add('='); Add(dwMajorVersion); Add('.'); Add(dwMinorVersion); Add('.');
    Add(dwBuildNumber);
    AddShort(' Wow64='); Add(integer(IsWow64));
    AddShort(' Freq='); Add(fFrequencyTimeStamp);
    if IsLibrary then begin
      AddShort(' Instance='); AddNoJSONEscapeString(InstanceFileName);
    end;
    Add(#13);
    AddClassName(self.ClassType); AddShort(' '+SYNOPSE_FRAMEWORK_VERSION+' ');
    AddDateTime(Now); Add(#13,#13);
  end;
  QueryPerformanceCounter(fStartTimeStamp);
  fHeaderWritten := true;
end;

procedure TSynLog.CreateLocker;
begin
  if fLockCS<>nil then
    exit;
  GetMem(fLockCS,sizeof(fLockCS^));
  InitializeCriticalSection(fLockCS^);
end;

procedure TSynLog.Lock;
begin
  if fLockCS<>nil then
    EnterCriticalSection(fLockCS^);
end;

procedure TSynLog.UnLock;
begin
  if fLockCS<>nil then
    LeaveCriticalSection(fLockCS^);
end;

{$ifndef DELPHI5OROLDER}
{$WARN SYMBOL_DEPRECATED OFF} // for GetHeapStatus
procedure TSynLog.AddMemoryStats;
begin
  with GetHeapStatus do
    if TotalAddrSpace<>0 then
    fWriter.Add(' AddrSpace=% Uncommitted=% Committed=% Allocated=% Free=% '+
       'FreeSmall=% FreeBig=% Unused=% Overheap=% ',
      [TotalAddrSpace,TotalUncommitted,TotalCommitted,TotalAllocated,TotalFree,
       FreeSmall,FreeBig,Unused,Overhead]);
end;
{$WARN SYMBOL_DEPRECATED ON}
{$endif}

procedure TSynLog.AddErrorMessage;
var Error, Len: Integer;
    Buffer: array[byte] of AnsiChar;
const LANG_ENGLISH = 5;
begin
  Error := GetLastError;
  Len := FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM or FORMAT_MESSAGE_ARGUMENT_ARRAY,
    nil, Error, LANG_ENGLISH, Buffer, SizeOf(Buffer), nil);
  while (Len>0) and (Buffer[Len-1] in [#0..' ','.']) do dec(Len);
  Buffer[Len] := #0;
  fWriter.AddOnSameLine(@Buffer);
  fWriter.Add(' ','(');
  fWriter.Add(Error);
  fWriter.Add(')',' ');
end;

procedure TSynLog.LogHeader(Level: TSynLogInfo);
var i: integer;
begin
  if fWriter=nil then
    CreateLogWriter
  else if  (assigned(fWriterStream))
      and (fWriterStream.Size >= fFamily.MaxMBSize * 1048576)
      and (fRecursionCount = 0) then
  begin
    fWriter.Flush;
    if fWriterStream.InheritsFrom(TFileStream) then
      FlushFileBuffers(TFileStream(fWriterStream).Handle);
    FreeAndNil(fWriterStream);
    fWriterStream := nil;
    CreateLogWriter;
    fHeaderWritten := false;
  end;

  if not fHeaderWritten then
    LogFileHeader;
  if not (sllEnter in fFamily.Level) and (Level in LOG_STACKTRACE) then
     for i := 0 to fRecursionCount-1 do
     begin
       fWriter.AddChars(' ',i+24-byte(fFamily.HighResolutionTimeStamp));
       AddRecursion(i,sllNone);
     end;
  if fFamily.HighResolutionTimeStamp and (fFrequencyTimeStamp<>0) then
  begin
    QueryPerformanceCounter(fCurrentTimeStamp);
    dec(fCurrentTimeStamp,fStartTimeStamp);
    fWriter.AddBinToHexDisplay(@fCurrentTimeStamp,sizeof(fCurrentTimeStamp));
  end else
    fWriter.AddCurrentLogTime;
  fWriter.AddShort(LOG_LEVEL_TEXT[Level]);
  fWriter.AddChars(#9,fRecursionCount);
  case Level of // handle additional text for some special error levels
{$ifndef DELPHI5OROLDER}
    sllMemory:     AddMemoryStats;
{$endif}
    sllLastError:  AddErrorMessage;
  end;
end;

procedure TSynLog.LogTrailer(Level: TSynLogInfo);
begin
  if Level in fFamily.fLevelStackTrace then
    AddStackTrace(nil);
  fWriter.AddCR;
end;

{$ifndef DELPHI5OROLDER}
procedure TSynLog.LogInternal(Level: TSynLogInfo; TextFmt: PWinAnsiChar;
  const TextArgs: array of const; Instance: TObject);
begin
  Lock;
  try
    LogHeader(Level);
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ');
    fWriter.Add(TextFmt,TextArgs,twOnSameLine);
    LogTrailer(Level);
  finally
    Unlock;
  end;
end;
{$endif}

procedure TSynLog.LogInternal(Level: TSynLogInfo; const Text: RawUTF8;
  Instance: TObject);
begin
  Lock;
  try
    LogHeader(Level);
    if Text='' then
    begin
      if Instance<>nil then
        if Instance.InheritsFrom(Exception) then
        begin
          fWriter.AddInstanceName(Instance,':');
          fWriter.Add('"');
          fWriter.AddJSONEscapeString(Exception(Instance).Message);
          fWriter.Add('"');
        end else
          fWriter.WriteObject(Instance,false,false,true);
    end else begin
      if Instance<>nil then
        fWriter.AddInstancePointer(Instance,' ');
      fWriter.AddOnSameLine(pointer(Text));
    end; 
    LogTrailer(Level);
  finally
    Unlock;
  end;
end;

procedure TSynLog.AddTyped(aTypeInfo: pointer; var aValue);
begin
  case PByte(aTypeInfo)^ of
    tkClass:
      fWriter.WriteObject(TObject(aValue),false,false,true);
    tkEnumeration:
{$ifdef PUREPASCAL}
      fWriter.Add(byte(aValue));
{$else}
      fWriter.AddShort(GetEnumName(aTypeInfo,byte(aValue))^);
{$endif}
    tkDynArray:
      fWriter.AddDynArrayJSON(DynArray(aTypeInfo,aValue));
  end;
end;

procedure TSynLog.LogInternal(Level: TSynLogInfo; aName: PWinAnsiChar;
   aTypeInfo: pointer; var aValue; Instance: TObject=nil);
begin
  Lock;
  try
    LogHeader(Level);
    if Instance<>nil then
      fWriter.AddInstancePointer(Instance,' ');
    fWriter.AddNoJSONEscape(aName);
    fWriter.Add('=');
    AddTyped(aTypeInfo,aValue); 
    LogTrailer(Level);
  finally
    Unlock;
  end;
end;

procedure TSynLog.CreateLogWriter;
begin
  {if (fRecursionCount = 0)
    and (assigned(fWriterStream)) 
    and (fWriterStream.Size >= fFamily.MaxMBSize * 1048576) then
  begin
    self.Flush(true);
    FreeAndNil(fWriterStream);
    fWriterStream := nil;
  end;  }
  if fWriterStream=nil then
  begin
    ExeVersionRetrieve;
    fFileName := Ansi7ToString(ExeVersion.ProgramName);
    fFileName := fFileName+' '+Ansi7ToString(NowToString(false));
    if IsLibrary then
      fFileName := fFileName+' '+ExtractFileName(GetModuleName(HInstance));
    fFileName := fFamily.fDestinationPath+fFileName+fFamily.fDefaultExtension;
    TFileStream.Create(fFileName,fmCreate).Free; // create a void file
    fWriterStream := TFileStream.Create(fFileName, // open it with read sharing
      fmOpenReadWrite or fmShareDenyWrite);
  end;
  if fWriter=nil then
    fWriter := TTextWriter.Create(fWriterStream,fFamily.BufferSize)
  else
   fWriter.Stream := fWriterStream;
end;

procedure TSynLog.AddRecursion(aIndex: integer; aLevel: TSynLogInfo);
type
  TTypeInfo = packed record
    Kind: byte;
    Name: ShortString;
  end;
  TClassType = packed record
     ClassType: TClass;
     ParentInfo: pointer;
     PropCount: SmallInt;
     UnitName: ShortString;
  end;
  PTypeInfo = ^TTypeInfo;
  PClassType = ^TClassType;
var Info: PTypeInfo;
label DoEnt;
begin
  if cardinal(aIndex)<cardinal(fRecursionMax) then
  with fRecursion^[aIndex] do begin
    if ClassType<>nil then begin
      if fFamily.WithUnitName then begin
        Info := PPointer(PtrInt(ClassType)+vmtTypeInfo)^;
        if Info<>nil then begin
          fWriter.AddShort(PClassType(@Info^.Name[ord(Info^.Name[0])+1])^.UnitName);
          fWriter.Add('.');
        end;
      end;
      fWriter.AddShort(PShortString(PPointer(PtrInt(ClassType)+vmtClassName)^)^);
      if Instance<>nil then begin
        fWriter.Add('(');
        fWriter.AddPointer(PtrUInt(Instance));
        fWriter.Add(')');
      end;
      fWriter.Add('.');
    end;
    if Method<>nil then
      fWriter.AddNoJSONEscape(Method)
    else
      TSynMapFile.Log(fWriter,Caller);
    if fFamily.HighResolutionTimeStamp and (fFrequencyTimeStamp<>0) then
      DoEnt:
      case aLevel of
        sllEnter:
          EnterTimeStamp := fCurrentTimeStamp;
        sllLeave:
        begin
          fWriter.Add(' ');
          fWriter.AddMicroSec(((fCurrentTimeStamp-EnterTimeStamp)*(1000*1000))div fFrequencyTimeStamp);
        end;
      end else
    if aLevel in [sllEnter,sllLeave] then
    begin
      QueryPerformanceCounter(fCurrentTimeStamp);
      dec(fCurrentTimeStamp,fStartTimeStamp);
      goto DoEnt;
    end;
  end;
  fWriter.AddCR;
end;

procedure TSynLog.DoEnterLeave(aLevel: TSynLogInfo);
begin
  Lock;
  try
    LogHeader(aLevel);
    AddRecursion(fRecursionCount,aLevel);
  finally
    UnLock;
  end;
end;

procedure TSynLog.AddStackTrace(Stack: PPtrUInt);
{$ifndef FPC}
{$ifndef CPU64}
procedure AddStackManual(Stack: PPtrUInt);
  function check2(xret: PtrUInt): Boolean;
  var i: PtrUInt;
  begin
    result := true;
    for i := 2 to 7 do
      if PWord(xret-i)^ and $38FF=$10FF then
        exit;
    result := false;
  end;
var st, max_stack, min_stack, depth: PtrUInt;
    prevState: boolean;
begin
  depth := fFamily.StackTraceLevel;
  if Stack=nil then // if no Stack pointer set, retrieve current one
    asm
      mov eax,ebp // push ebp; mov ebp,esp done at begin level above
      mov Stack,eax
    end;
  asm
    mov eax,fs:[18h]
    mov ecx,dword ptr [eax+4]
    mov max_stack,ecx
    mov ecx,dword ptr [eax+8]
    mov min_stack,ecx
  end;
  fWriter.AddShort(' stack trace ');
  prevState := SynLogExceptionEnabled;
  SynLogExceptionEnabled := false; // for IsBadCodePtr
  try
    try
      while (PtrUInt(stack)<max_stack) do
      begin
        st := stack^;
        if ((st>max_stack) or (st<min_stack)) and
           not IsBadReadPtr(pointer(st-8),12) and
           ((pByte(st-5)^=$E8) or check2(st)) then
        begin
          TSynMapFile.Log(fWriter,st); // will ignore any TSynLog.* methods
          dec(depth);
          if depth=0 then
            break;
        end;
        inc(stack);
      end;
    except
      // just ignore any access violation here
    end;
  finally
    SynLogExceptionEnabled := prevState;
  end;
end;
{$endif}
{$endif}
var n, i: integer;
    prevState: boolean;
    BackTrace: array[byte] of PtrUInt;
begin
  if fFamily.StackTraceLevel<=0 then
    exit;
  if RtlCaptureStackBackTraceRetrieved<>btOK then
  begin
{$ifndef FPC}
{$ifndef CPU64}
    AddStackManual(Stack);
{$endif}
{$endif}
  end else
  begin
    prevState := SynLogExceptionEnabled;
    SynLogExceptionEnabled := false; // for IsBadCodePtr
    try
      try
        n := RtlCaptureStackBackTrace(2,fFamily.StackTraceLevel,@BackTrace,nil);
        if n=0 then begin
          {$ifndef FPC}
          {$ifndef CPU64}
          if not fFamily.StackTraceUseOnlyAPI then
            AddStackManual(Stack);
          {$endif}
          {$endif}
        end else begin
          fWriter.AddShort(' stack trace API ');
          for i := 0 to n-1 do
            TSynMapFile.Log(fWriter,BackTrace[i]); // will ignore any TSynLog.* methods
        end;
      except
        // just ignore any access violation here
      end;
    finally
      SynLogExceptionEnabled := prevState;
    end;
  end;
end;

procedure ExeVersionRetrieve(DefaultVersion: integer);
const EXE_FMT: PUTF8Char = '% % (%)';
var Tmp: array[byte] of AnsiChar;
    TmpSize: cardinal;
    i: integer;
begin
  with ExeVersion do
  if Version=nil then begin
    ProgramFileName := paramstr(0);
    ProgramFilePath := ExtractFilePath(ProgramFileName);
    if IsLibrary then 
      InstanceFileName := GetModuleName(HInstance) else
      InstanceFileName := ProgramFileName;
    Version := TFileVersion.Create(InstanceFileName,DefaultVersion);
    GarbageCollector.Add(Version);
    ProgramFullSpec := FormatUTF8(EXE_FMT,
      [ProgramFileName,Version.Detailed,DateTimeToIso8601(Version.BuildDateTime,True,' ')]);
    ProgramName := StringToUTF8(ExtractFileName(ProgramFileName));
    i := length(ProgramName);
    while i>0 do
      if ProgramName[i]='.' then begin
        SetLength(ProgramName,i-1);
        break;
      end else
      dec(i);
    TmpSize := sizeof(Tmp);
    GetComputerNameA(Tmp,TmpSize);
    Host := Tmp;
    TmpSize := sizeof(Tmp);
    GetUserNameA(Tmp,TmpSize);
    User := Tmp;//StringToUTF8(Tmp);
  end;
end;


{ TMemoryMapText }

constructor TMemoryMapText.Create(aFileContent: PUTF8Char; aFileSize: integer);
begin
  fMap.Map(aFileContent,aFileSize);
  LoadFromMap;
end;

constructor TMemoryMapText.Create(const aFileName: TFileName);
begin
  fFileName := aFileName;
  if fMap.Map(aFileName) then
    LoadFromMap;
end; // invalid file or unable to memory map its content -> Count := 0

destructor TMemoryMapText.Destroy;
begin
  Freemem(fLines);
  fMap.UnMap;
  inherited;
end;

function TMemoryMapText.GetLine(aIndex: integer): RawUTF8;
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) then
    result := '' else
    SetString(result,PAnsiChar(fLines[aIndex]),GetLineSize(fLines[aIndex],fMapEnd));
end;

function TMemoryMapText.GetString(aIndex: integer): string;
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) then
    result := '' else
    //result := UTF8DecodeToString(fLines[aIndex],GetLineSize(fLines[aIndex],fMapEnd));
   SetString(result,PAnsiChar(fLines[aIndex]),GetLineSize(fLines[aIndex],fMapEnd));
end;

function GetLineContains(p,pEnd, up: PUTF8Char): boolean; ///{$ifdef HASINLINE}inline;{$endif}
var i: integer;
label Fnd;
begin // fast unrolled search
  repeat
    if p>=pEnd then break;
    i := ord(p^);
    if not (AnsiChar(i) in ANSICHARNOT01310) then
      break;
    if NormToUpperAnsi7Byte[i]=ord(up^) then
      goto Fnd;
    inc(p);
    if p>=pEnd then
      break;
    i := ord(p^);
    if not (AnsiChar(i) in ANSICHARNOT01310) then
      break;
    if NormToUpperAnsi7Byte[i]=ord(up^) then
      goto Fnd;
    inc(p);
    if p>=pEnd then
      break;
    i := ord(p^);
    if not (AnsiChar(i) in ANSICHARNOT01310) then
      break;
    if NormToUpperAnsi7Byte[i]=ord(up^) then
      goto Fnd;
    inc(p);
    if p>=pEnd then
      break;
    i := ord(p^);
    if not (AnsiChar(i) in ANSICHARNOT01310) then
      break;
    if NormToUpperAnsi7Byte[i]<>ord(up^) then
    begin
      inc(p);
      Continue;
    end;
Fnd:i := 0;
    repeat
      inc(i);
      if up[i]=#0 then
      begin
        result := true; // found
        exit;
      end;
    until NormToUpperAnsi7[p[i]]<>up[i];
    inc(p);
  until false;
  result := false;
end;

function TMemoryMapText.LineContains(const aUpperSearch: RawUTF8;
  aIndex: Integer): Boolean;
begin
  if (self=nil) or (cardinal(aIndex)>=cardinal(fCount)) or (aUpperSearch='') then
    result := false
  else
    result := GetLineContains(fLines[aIndex],fMapEnd,pointer(aUpperSearch));
end;

function TMemoryMapText.LineSize(aIndex: integer): integer;
begin
  result := GetLineSize(fLines[aIndex],fMapEnd);
end;

function GetLineSizeSmallerThan(P,PEnd: PUTF8Char; aMinimalCount: integer): boolean;
begin
  if P<>nil then
    while (P<PEnd) and (P^ in ANSICHARNOT01310) do
      if aMinimalCount=0 then begin
        result := false;
        exit;
      end else begin
        dec(aMinimalCount);
        inc(P);
      end;
  result := true;
end;

function TMemoryMapText.LineSizeSmallerThan(aIndex, aMinimalCount: integer): boolean;
begin
  result := GetLineSizeSmallerThan(fLines[aIndex],fMapEnd,aMinimalCount);
end;

procedure TMemoryMapText.ProcessOneLine(LineBeg, LineEnd: PUTF8Char; max: Integer);
begin
  fLines[fCount] := LineBeg;
  inc(fCount);
  if fCount=max then begin
    max := max+256+max shr 3;
    Reallocmem(fLines,max*sizeof(pointer));
  end;
end;

procedure TMemoryMapText.LoadFromMap;
var max: integer;
    P, PBeg: PUTF8Char;
begin
  max := fMap.fFileSize shr 5+8; // guess from average line length of 32 chars
  Getmem(fLines,max*sizeof(pointer));
  P := pointer(fMap.Buffer);
  fMapEnd := P+fMap.Size;
  if (Map.Size>3) and (PCardinal(P)^ and $ffffff=$bfbbef) then
    inc(PByte(P),3); // ignore UTF-8 BOM
  while P<fMapEnd do begin
    PBeg := P;
    while (P^<>#13) and (P^<>#10) do
      if P=fMapEnd then
        break else
        inc(P);
    ProcessOneLine(PBeg,P,max);
    if P+1>=fMapEnd then
      break;
    if P[0]=#13 then
      if P[1]=#10 then
        inc(P,2) else // ignore #13#10
        inc(P) else   // ignore #13
      inc(P);         // ignore #10
  end;
  if max>fCount+256 then
    Reallocmem(fLines,fCount*sizeof(pointer)); // size down only if worth it
end;


{ TSynLogFile }

function TSynLogFile.EventCount(const aSet: TSynLogInfos): integer;
var i: integer;
begin
  result := 0;
  for i := 0 to Count-1 do
    if fLevels[i] in aSet then
      inc(result);
end;

function TSynLogFile.EventDateTime(aIndex: integer): TDateTime;
var TimeStamp: Int64;
begin
  if cardinal(aIndex)>=cardinal(fCount) then
    result := 0
  else if fFreq=0 then
    Iso8601ToDateTimePUTF8CharVar(fLines[aIndex],17,result)
  else if HexDisplayToBin(fLines[aIndex],@TimeStamp,sizeof(TimeStamp)) then
    result := fStartDateTime+(TimeStamp/fFreqPerDay)
  else
    result := 0;
end;

procedure TSynLogFile.LoadFromMap;
  var PBeg, P, PEnd: PUTF8Char;
  function StrPosI(P,PEnd,SearchUp: PUTF8Char): PUTF8Char;
  begin
    result := P;
    while result<PEnd do
      if IdemPChar(result,SearchUp) then
        exit else
        inc(result);
    result := nil;
  end;
  function GetOne(const UP: RawUTF8; var S: RawUTF8): boolean;
  var LUP: integer;
  begin
    LUP := length(UP);
    P := StrPosI(PBeg,PEnd-LUP,pointer(UP));
    if P=nil then
      result := false else begin
      SetString(S,PAnsiChar(PBeg),P-PBeg);
      PBeg := P+LUP;
      result := pointer(S)<>nil;
    end;
  end;
  function ComputeProperTime(var procndx: Integer): cardinal; // returns leave
  var start, i: integer;
  begin
    start := procndx;
    with fLogProcNatural[procndx] do begin
      ProperTime := Time;
      result := Index;
    end;
    repeat
      inc(result);
      if result>=Cardinal(Count) then
        break;
      case fLevels[result] of
      sllEnter: begin
        inc(procndx);
        assert(fLogProcNatural[procndx].Index=result);
        result := ComputeProperTime(procndx);
      end;
      sllLeave: begin
        with fLogProcNatural[start] do
        for i := start+1 to procndx do
          dec(ProperTime,fLogProcNatural[i].ProperTime);
        break;
      end;
      end;
    until false;
  end;
var aWow64: RawUTF8;
    aCount, i, pCount, j, Level: integer;
    TSEnter, TSLeave: Int64;
    L: TSynLogInfo;
begin
  // 1. calculate fLines[] + fCount and fLevels[] + fLogProcNatural[] from .log content
  for L := low(TSynLogInfo) to high(TSynLogInfo) do // needed by ProcessOneLine
    fLogLevelsTextMap[L] := PCardinal(@LOG_LEVEL_TEXT[L][3])^; // [3] -> e.g. 'UST4'
  inherited;
  // 2. fast retrieval of header
  aCount := fCount;
  fCount := 0; // leave Count=0 if incorrect format
{  C:\Dev\lib\SQLite3\exe\TestSQL3.exe 0.0.0.0 (2011-04-07 11:09:06)
   Host=BW013299 User=G018869 CPU=1*0-15-1027 OS=2.3=5.1.2600 Wow64=0 Freq=3579545
   TSynLog 1.13 LVCL 2011-04-07 12:04:09 }
  if (aCount<=4) or LineSizeSmallerThan(0,24) or not IdemPChar(fLines[1],'HOST=') or
     LineSizeSmallerThan(2,24) or (LineSize(3)<>0) or LineSizeSmallerThan(4,16) then
    exit;
  PBeg := fLines[0];
  PEnd := PBeg+LineSize(0)-12;
  if PEnd<PBeg then
    exit;
  if PEnd^='(' then begin  // '(2011-04-07)' format
    if (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[11]<>')') then
      exit;
    Iso8601ToDateTimePUTF8CharVar(PEnd+1,10,fExeDate);
  end else begin  // '(2011-04-07 11:09:06)' format
    dec(PEnd,9);
    if (PEnd<PBeg) or (PEnd[-1]<>' ') or (PEnd[0]<>'(') or (PEnd[20]<>')') then
      exit;
    Iso8601ToDateTimePUTF8CharVar(PEnd+1,19,fExeDate);
  end;
  dec(PEnd);
  P := PEnd;
  repeat if P<=PBeg then exit else dec(P) until P^=' ';
  SetString(fExeVersion,PAnsiChar(P+1),PEnd-P-1);
  repeat dec(P); if P<=PBeg then exit; until P^<>' ';
  SetString(fExeName,PAnsiChar(PBeg),P-PBeg+1);
  PBeg := PUTF8Char(fLines[1])+5;
  PEnd := PUTF8Char(fLines[1])+LineSize(1);
  if not GetOne(' USER=',fHost) or not GetOne(' CPU=',fUser) or
     not GetOne(' OS=',fCPU)    or not GetOne(' WOW64=',fOsDetailed) or
     not GetOne(' FREQ=',aWow64) then
    exit;
  fWow64 := aWow64='1';
  SetInt64(PBeg,fFreq);
  if fFreq=0 then
    exit;
  while (PBeg<PEnd) and (PBeg^>' ') do inc(PBeg);
  if IdemPChar(PBeg,' INSTANCE=') then // only available for a library log
    SetString(fInstanceName,PBeg+10,PEnd-PBeg-10);
  if PWord(fLines[4])^<>ord('0')+ord('0')shl 8 then // YYYYMMDD -> 20101225 e.g.
    fFreq := 0 else // =0 if date time, >0 if high-resolution time stamp
    fFreqPerDay := fFreq*SecsPerDay;
  Iso8601ToDateTimePUTF8CharVar(PUTF8Char(fLines[2])+LineSize(2)-19,19,fStartDateTime);
  if fStartDateTime=0 then
    exit;
  P := pointer(fOSDetailed);
  fOS := TWindowsVersion(GetNextItemCardinal(P,'.'));
  fOSServicePack := GetNexTItemCardinal(P);
  // 3. compute fCount and fLines[] so that all fLevels[]<>sllNone
  fCount := aCount;
  pCount := 0;
  aCount := 0;
  for i := 0 to fCount-1 do
    if fLevels[i]<>sllNone then begin
      fLevels[aCount] := fLevels[i];
      fLines[aCount] := fLines[i];
      if fLevels[i]=sllEnter then begin
        fLogProcNatural[pCount].Index := aCount;
        inc(pCount);
      end;
      inc(aCount);
    end;
  fCount := aCount;
  assert(pCount=fLogProcNaturalCount);
  SetLength(fLogProcNatural,pCount);
  for i := 0 to pCount-1 do
    if fLogProcNatural[i].Time>=99000000 then begin // overange 99.000.000 -> compute
      Level := 0;
      j := fLogProcNatural[i].Index;
      repeat
        inc(j);
        if j=aCount then break;
        case fLevels[j] of
        sllEnter: inc(Level);
        sllLeave: if Level=0 then begin
          if fFreq=0 then // adjust huge seconds timing from date/time column
            fLogProcNatural[i].Time :=
              Round((EventDateTime(j)-EventDateTime(fLogProcNatural[i].Index))*86400000000.0)+
              fLogProcNatural[i].Time mod 1000000 else begin
            HexDisplayToBin(fLines[fLogProcNatural[i].Index],@TSEnter,sizeof(TSEnter));
            HexDisplayToBin(fLines[j],@TSLeave,sizeof(TSLeave));
            fLogProcNatural[i].Time := ((TSLeave-TSEnter)*(1000*1000)) div fFreq;
          end;
          break;
        end else dec(Level);
        end;
      until false;
    end;
  i := 0;
  while i<pCount do begin
    ComputeProperTime(i);
    inc(i);
  end;
  LogProcMerged := false; // set LogProp[]
end;

procedure TSynLogFile.LogProcSort(Order: TLogProcSortOrder);
begin
  if (fLogProcNaturalCount<=1) or (Order=fLogProcSortInternalOrder) then
    Exit;
  fLogProcSortInternalOrder := Order;
  LogProcSortInternal(0,LogProcCount-1);
end;

function StrICompLeftTrim(Str1, Str2: PUTF8Char): PtrInt;
var C1, C2: integer;
begin
  while Str1^ in [#9,' '] do inc(Str1);
  while Str2^ in [#9,' '] do inc(Str2);
  repeat
    C1 := NormToUpperByte[ord(Str1^)];
    C2 := NormToUpperByte[ord(Str2^)];
    if (C1<>C2) or (C1<32) then
      break;
    Inc(Str1);
    Inc(Str2);
  until false;
  Result := C1-C2;
end;

function TSynLogFile.LogProcSortComp(A, B: Integer): integer;
begin
  case fLogProcSortInternalOrder of
    soByName: result :=
      StrICompLeftTrim(PUTF8Char(fLines[LogProc[A].Index])+22,
                       PUTF8Char(fLines[LogProc[B].Index])+22);
    soByOccurrence: result := LogProc[A].Index-LogProc[B].Index;
    soByTime:       result := LogProc[B].Time-LogProc[A].Time;
    soByProperTime: result := LogProc[B].ProperTime-LogProc[A].ProperTime;
    else  result := A-B;
  end;
end;

procedure TSynLogFile.LogProcSortInternal(L, R: integer);
var I,J,P: integer;
begin
  if L<R then
  repeat
    I := L; J := R;
    P := (L + R) shr 1;
    repeat
      while LogProcSortComp(I,P)<0 do inc(I);
      while LogProcSortComp(J,P)>0 do dec(J);
      if I<=J then begin
        Exchg(@LogProc[i],@LogProc[j],sizeof(LogProc[i])-1);
        if P = I then P := J else if P = J then P := I;
        Inc(I); Dec(J);
      end;
    until I>J;
    if L<J then
      LogProcSortInternal(L,J);
     L := I;
  until I>=R;
end;

procedure TSynLogFile.ProcessOneLine(LineBeg, LineEnd: PUTF8Char; max: integer);
  function DecodeMicroSec(P: PByte): integer;
  var B: integer;
  begin // fast decode 00.020.006 at the end of the line
    B := ConvertHexToBin[P^];   // 00
    if B>9 then
      result := -1 else begin
      result := B;
      inc(P);
      B := ConvertHexToBin[P^];
      if B>9 then
        result := -1 else begin
        result := result*10+B;
        inc(P,2);                 // .
        B := ConvertHexToBin[P^]; // 020
        if B>9 then
          result := -1 else begin
          result := result*10+B;
          inc(P);
          B := ConvertHexToBin[P^];
          if B>9 then
            result := -1 else begin
            result := result*10+B;
            inc(P);
            B := ConvertHexToBin[P^];
            if B>9 then
              result := -1 else begin
              result := result*10+B;
              inc(P,2);                 // .
              B := ConvertHexToBin[P^]; // 006
              if B>9 then
                result := -1 else begin
                result := result*10+B;
                inc(P);
                B := ConvertHexToBin[P^];
                if B>9 then
                  result := -1 else begin
                  result := result*10+B;
                  inc(P);
                  B := ConvertHexToBin[P^];
                  if B>9 then
                    result := -1 else
                    result := result*10+B;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
var V: cardinal;
    MS: integer;
    L: TSynLogInfo;
begin
  inherited;
  if length(fLevels)<max then
    SetLength(fLevels,max);
  if (fCount<4) or (LineEnd-LineBeg<24) then
    exit;
  if LineBeg[8]=' ' then // YYYYMMDD HHMMSS one character bigger than Time Stamp
    V := PCardinal(LineBeg+19)^ else
    V := PCardinal(LineBeg+18)^;
  for L := succ(sllNone) to high(TSynLogInfo) do
    if V=fLogLevelsTextMap[L] then begin
      fLevels[fCount-1] := L; // need exact match of level text
      include(fLevelUsed,L);
      case L of
      sllEnter: begin
        if Cardinal(fLogProcStackCount)>=Cardinal(length(fLogProcStack)) then
          SetLength(fLogProcStack,length(fLogProcStack)+256);
        fLogProcStack[fLogProcStackCount] := fLogProcNaturalCount;
        inc(fLogProcStackCount);
        if Cardinal(fLogProcNaturalCount)>=Cardinal(length(fLogProcNatural)) then
          SetLength(fLogProcNatural,length(fLogProcNatural)+32768);
        // fLogProcNatural[].Index will be set in TSynLogFile.LoadFromMap
        inc(fLogProcNaturalCount);
      end;
      sllLeave:
      if (LineEnd-LineBeg>10) and (LineEnd[-4]='.') and (LineEnd[-8]='.') and
         (fLogProcStackCount>0) then begin // 00.020.006
        MS := DecodeMicroSec(PByte(LineEnd-10));
        if MS>=0 then begin
          dec(fLogProcStackCount);
          fLogProcNatural[fLogProcStack[fLogProcStackCount]].Time := MS;
        end;
      end;
      end;
      break;
    end;
end;

procedure TSynLogFile.SetLogProcMerged(const Value: boolean);
var i: integer;
    P: ^TSynLogFileProc;
    O: TLogProcSortOrder;
begin
  fLogProcIsMerged := Value;
  O := fLogProcSortInternalOrder;
  if Value then begin
    if fLogProcMerged=nil then begin
      fLogProcCurrent := pointer(fLogProcNatural);
      fLogProcCurrentCount := fLogProcNaturalCount;
      LogProcSort(soByName); // sort by name to identify unique
      SetLength(fLogProcMerged,fLogProcNaturalCount);
      fLogProcMergedCount := 0;
      i := 0;
      P := pointer(fLogProcNatural);
      repeat
        with fLogProcMerged[fLogProcMergedCount] do begin
          repeat
            Index := P^.Index;
            inc(Time,P^.Time);
            inc(ProperTime,P^.ProperTime);
            inc(i);
            inc(P);
          until (i>=fLogProcNaturalCount) or
                (StrICompLeftTrim(PUTF8Char(fLines[LogProc[i-1].Index])+22,
                 PUTF8Char(fLines[P^.Index])+22)<>0);
        end;
        inc(fLogProcMergedCount);
      until i>=fLogProcNaturalCount;
      SetLength(fLogProcMerged,fLogProcMergedCount);
    end;
    fLogProcCurrent := pointer(fLogProcMerged);
    fLogProcCurrentCount := fLogProcMergedCount;
  end else begin
    fLogProcCurrent := pointer(fLogProcNatural);
    fLogProcCurrentCount := fLogProcNaturalCount;
  end;
  fLogProcSortInternalOrder := soNone;
  LogProcSort(O); // restore previous sort order
end;


function EventArchiveDelete(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin
  result := DeleteFile(aOldLogFileName);
end;

function EventArchiveSynLZ(const aOldLogFileName, aDestinationPath: TFileName): boolean;
begin // aDestinationPath = 'ArchivePath\log\YYYYMM\'
  Result := false;
  if (aOldLogFileName<>'') and FileExists(aOldLogFileName)  then
  try
    if DirectoryExists(aDestinationPath) or CreateDir(aDestinationPath) then
      if FileSynLZ(aOldLogFileName,
         aDestinationPath+ExtractFileName(aOldLogFileName)+'.synlz',LOG_MAGIC) then
        result := DeleteFile(aOldLogFileName);
  except
    on Exception do
      result := false;
  end;
end;


{ TRawByteStringStream }

constructor TRawByteStringStream.Create(const aString: RawByteString);
begin
  if aString<>'' then
    fDataString := aString;
end;

function TRawByteStringStream.Read(var Buffer; Count: Integer): Longint;
begin
  if Count<=0 then
    Result := 0 else begin
    Result := Length(fDataString)-fPosition;
    if Result>Count then
      Result := Count;
    move(PByteArray(fDataString)[fPosition],Buffer,Result);
    inc(fPosition, Result);
  end;
end;

function TRawByteStringStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: fPosition := Offset;
    soFromCurrent:   fPosition := fPosition+Offset;
    soFromEnd:       fPosition := Length(fDataString)-Offset;
  end;
  if fPosition>Length(fDataString) then
    fPosition := Length(fDataString) else
    if fPosition<0 then
      fPosition := 0;
  Result := fPosition;
end;

procedure TRawByteStringStream.SetSize(NewSize: Integer);
begin
  SetLength(fDataString, NewSize);
  if fPosition>NewSize then
    fPosition := NewSize;
end;

function TRawByteStringStream.Write(const Buffer; Count: Integer): Longint;
begin
  if Count<=0 then
    Result := 0 else begin
    Result := Count;
    SetLength(fDataString,(fPosition+Result));
    move(Buffer,PByteArray(fDataString)[fPosition],Result);
    inc(FPosition,Result);
  end;
end;


{ TSynNameValue }

procedure TSynNameValue.Add(const aName, aValue: RawUTF8; aTag: PtrInt);
var added: boolean;
    i: Integer;
begin
  i := fDynArray.FindHashedForAdding(aName,added);
  with List[i] do begin
    if added then
      Name := aName;
    Value := aValue;
    Tag := aTag;
  end;
end;

procedure TSynNameValue.Init(aCaseSensitive: boolean);
begin
  fDynArray.Init(
    TypeInfo(TSynNameValueItemDynArray),List,nil,nil,nil,@Count,not aCaseSensitive);
end;

function TSynNameValue.Find(const aName: RawUTF8): integer;
begin
  result := fDynArray.FindHashed(aName);
end;

function TSynNameValue.Value(const aName: RawUTF8): RawUTF8;
var i: integer;
begin
  i := fDynArray.FindHashed(aName);
  if i<0 then
    result := '' else
    result := List[i].Value;
end;

function TSynNameValue.Initialized: boolean;
begin
  result := fDynArray.Value<>nil;
end;

function TSynNameValue.GetBlobData: RawByteString;
begin
  result := fDynArray.SaveTo;
end;

procedure TSynNameValue.SetBlobData(const aValue: RawByteString);
begin
  fDynArray.LoadFrom(pointer(aValue));
  fDynArray.ReHash;
end;

{$ifndef DELPHI5OROLDER}
{$ifndef PUREPASCAL}
{$ifdef CPU64}
const
  SupportsSSE2 = true;
{$else}
var
  SupportsSSE2: boolean = false;

procedure TestIfSupportsSSE2;
asm
  pushfd
  pop eax
  mov edx,eax
  xor eax,$200000
  push eax
  popfd
  pushfd
  pop eax
  xor eax,edx
  jz @exit
  push ebx
  mov eax,1
  cpuid
  test edx,$04000000
  jz @no
  mov SupportsSSE2,true
@no:
  pop ebx
@exit:
end;
{$endif}
{$endif}
{$endif}

initialization
  GarbageCollector := TObjectList.Create;
  {$ifndef DELPHI5OROLDER}
   {$ifndef PUREPASCAL}
    {$ifndef CPU64}
  TestIfSupportsSSE2;
    {$endif CPU64}
   {$endif PUREPASCAL}
  {$endif DELPHI5OROLDER}
  {$ifndef FPC}
   {$ifndef ENHANCEDRTL}
    {$ifndef PUREPASCAL}
     {$ifndef DELPHI5OROLDER}
      {$ifndef USEPACKAGES}
       {$ifdef DOPATCHTRTL}
  RedirectCode(GetAddressFromCall(@RecordCopyInvoke),@RecordCopy);
       {$endif DOPATCHTRTL}
      {$endif USEPACKAGES}
     {$endif DELPHI5OROLDER}
     {$ifndef LVCL}
      {$ifndef DELPHI5OROLDER}
  if not SupportsSSE2 then // back to default X87 code for older CPUs
    PatchCode(@FillChar,@FillCharX87,FILLCHAR_SIZE);
      {$endif DELPHI5OROLDER}
      {$ifndef ISDELPHI2007ANDUP} // use faster FillChar/Move for older Delphi
  RedirectCode(GetAddressFromCall(@FillCharInvoke),@FillChar);
  RedirectCode(GetAddressFromCall(@MoveInvoke),@Move);
      {$endif ISDELPHI2007ANDUP}
     {$endif LVCL}
    {$endif PUREPASCAL}
   {$endif ENHANCEDRTL}
  {$endif FPC}
  InitSynCommonsConversionTables;

  {$ifdef MSWINDOWS}
  RetrieveSystemInfo;
  {$endif MSWINDOWS}

finalization
  {$ifndef NOEXCEPTIONINTERCEPT}
  SynLogExceptionEnabled := false;
  {$endif NOEXCEPTIONINTERCEPT}
  GarbageCollector.Free; // free all global instances (avoid memory leak)

end.


