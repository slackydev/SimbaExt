#ifndef MATCHTEMPL_HPP_INCLUDED
#define MATCHTEMPL_HPP_INCLUDED

#if defined _WIN32 || defined _WIN64
#include <windows.h>
#else
#include <dlfcn.h>
#endif

#ifdef _MSC
#define _CRT_SECURE_NO_WARNINGS
#endif


#include <cstdint>
#include <cstring>
#include <iostream>

#include <opencv2/highgui/highgui.hpp>
#include <opencv2/imgproc/imgproc.hpp>

using namespace cv;


/** PASCAL EXPORTS **/

#if defined _WIN32 || defined _WIN64
#define DLL_FUNC __declspec(dllexport)
extern HMODULE module;
#else
#define DLL_FUNC
#define __stdcall
#endif

static const char* PascalExports[] =
{
    "loadImage",		"Procedure cv_LoadImage(ImFile: PChar; var Mat: Pointer; var Width, Height:Int32);",
    "freeImage",		"Procedure cv_FreeImage(var Mat: Pointer);",
    "MatFromData",	"Procedure cv_MatFromData(Data: Pointer; Width, Height: Int32; var Mat: Pointer);",
    "matchTempl",		"Function cv_MatchTemplate(var Img, Templ: Pointer; MatchMethod: Int32; Normed: Boolean; var Mat: Pointer): Pointer;"
};

/*static const char* PascalTypes[] =
{
	"CVMatrix", "record Ptr: Pointer; Rows, Columns: UInt32; end;",  //exporting records..
	"CVMatrixClass", "type TObject;"    //exporting classes..
};*/

static const long int PascalExportCount = sizeof(PascalExports) / (sizeof(PascalExports[0]) * 2);
//static const long int PascalTypeCount = sizeof(PascalTypes) / (sizeof(PascalTypes[0]) * 2);

#ifdef __cplusplus
extern "C"
{
#endif

DLL_FUNC int GetPluginABIVersion();
DLL_FUNC int GetFunctionCount();
DLL_FUNC int GetFunctionInfo(int Index, void** Address, char** Definition);
/*DLL_FUNC int GetTypeCount();
DLL_FUNC int GetTypeInfo(int Index, char** Type, char** Definition);*/




DLL_FUNC void loadImage(char* imfile, Mat* &cvImage, int &Width, int &Height);
DLL_FUNC void freeImage(Mat* &cvImage);
DLL_FUNC void MatFromData(void* data, int width, int height, Mat* &result);
DLL_FUNC float* matchTempl(Mat* &templ, Mat* &im, int matchMethod, int normed, Mat* &result);

#ifdef __cplusplus
}
#endif

#endif // SLACKSTERCV_HPP_INCLUDED
