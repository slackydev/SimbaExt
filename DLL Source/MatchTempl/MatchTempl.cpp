#include "MatchTempl.hpp"


/**
  Procedure cv_LoadImage(ImFile: PChar; var Mat: Pointer; var Width, Height: Int32);
**/
void loadImage(char* imfile, Mat* &cvImage, int &Width, int &Height)
{
    cvImage = new Mat();
    *cvImage = imread(imfile, 1);

    if (cvImage->empty())
    {
        std::cout << "[Error] Cannot load image!" << std::endl;
        delete cvImage;
        cvImage = NULL;
        return;
    }

    Width = cvImage->cols;
    Height = cvImage->rows;
}


/**
  Procedure cv_FreeImage(var Mat: Pointer);
**/
void freeImage(Mat* &cvImage)
{
    if (cvImage)
    {
        cvImage->release();
        delete cvImage;
        cvImage = NULL;
    }
}


/**
  Procedure cv_MatFromData(Data: Pointer; Width, Height: Int32; var Mat: Pointer);
**/
void MatFromData(void* data, int width, int height, Mat* &result)
{
    result = new Mat();
    Mat* tmp = new Mat();
    *tmp = Mat(height, width, CV_8UC4, (unsigned char*)data);
    cvtColor(*tmp, *tmp, CV_BGRA2BGR);
    *result = tmp->clone();
    tmp->release();
    delete  tmp;
    tmp = NULL;
}


/**
  Function cv_MatchTemplate(var Img, Templ: Pointer; MatchMethod: Int32; Boolean: Normed; var Mat: Pointer): Pointer;
**/
float* matchTempl(Mat* &im, Mat* &templ, int matchMethod, int normed, Mat* &result)
{
    if ((matchMethod < 0) || (matchMethod > 5))
    {
        std::cout << "[Error] Undefined matchMethod! Should be in range of 0-5" << std::endl;
        return NULL;
    }

    if ((im->type() != templ->type()) || (im->depth() != templ->depth()))
    {
        std::cout << "[Error] " << std::endl;
        std::cout << "\t-> type:\t" << templ->type() << ", " << im->type() << std::endl;
        std::cout << "\t-> depth:\t" << templ->depth() << ", " << im->depth() << std::endl;
        return NULL;
    }

    result = new Mat();
    int result_cols = im->cols - templ->cols + 1;
    int result_rows = im->rows - templ->rows + 1;
    result->create( result_cols, result_rows, CV_32FC1 );

    matchTemplate(*im, *templ, *result, matchMethod);
    if(normed != 0)
        normalize(*result, *result, 0, 1, NORM_MINMAX, -1, Mat());

    return (float*)result->data;
}







/**                                     SIMBA EXPORTS                           **/

#if defined _WIN32 || defined _WIN64
HMODULE module = NULL;
#endif

int GetPluginABIVersion()
{
    return 2;
}

int GetFunctionCount()
{
    return PascalExportCount;
}

int GetFunctionInfo(int Index, void** Address, char** Definition)
{
    if (Index < PascalExportCount)
    {
        #if defined _WIN32 || defined _WIN64
        *Address = (void*)GetProcAddress(module, PascalExports[Index * 2]);
        #else
        *Address = (void*)dlsym(RTLD_DEFAULT, PascalExports[Index * 2]);
        #endif
        strcpy(*Definition, PascalExports[Index * 2 + 1]);
        return Index;
    }
    return -1;
}

/*int GetTypeCount()
{
    return PascalTypeCount;
}

int GetTypeInfo(int Index, char** Type, char** Definition)
{
    if (Index < PascalTypeCount)
    {
        strcpy(*Type, PascalTypes[Index * 2 + 0]);
        strcpy(*Definition, PascalTypes[Index * 2 + 1]);
        return Index;
    }
    return -1;
}
*/
