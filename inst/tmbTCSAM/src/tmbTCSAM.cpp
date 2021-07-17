#include <TMB.hpp>
#include <string>
const int DEBUG = 0;
#include "../include/tmbTCSAM.hpp"


template<class Type>
Type objective_function<Type>::operator() () {
    //get data objects
    DATA_INTEGER(debug);  //flag to print debugging info
    wtsPRINTSTR_DEBUG("Printing debugging information")

    DATA_STRUCT(stMC,structModelConfig);
    if (debug>2) wtsPRINTSTR_DEBUG("structModelConfig:")
    if (debug>2) wtsPRINTSTRUCT_DEBUG("mc = ",stMC)
    ModelConfig<Type>* pMC = ModelConfig<Type>::getInstance(stMC);
    if (debug>3) wtsPRINTSTR_DEBUG("class ModelConfig (testing local pointer):")
    if (debug>3) std::cout<<(*pMC)<<std::endl;
    if (debug>4) wtsPRINTSTR_DEBUG("class ModelConfig (testing static instance):")
    if (debug>4) std::cout<<(*(ModelConfig<Type>::getInstance()))<<std::endl;
    
    //for testing
    if (debug>2) wtsPRINTSTR_DEBUG("Printing testArray_zsmxray information")
    DATA_ARRAY(testArray_zsmxray);
    if (debug>2) wtsPRINTVAR_DEBUG("array dim = ",testArray_zsmxray.dim);
    if (debug>2) wtsPRINTVAR_DEBUG("array mlt = ",testArray_zsmxray.mult);
    if (debug>2) wtsPRINTVAR_DEBUG("ncols = ",testArray_zsmxray.cols());
    if (debug>2) wtsPRINTVAR_DEBUG("nrows = ",testArray_zsmxray.rows());

    //for testing
    vector<int> idx(7);//zsmxray
    {
        idx << 0,0,0,0,0,0,0; 
        std::string str = "idx("; str += "0,0,0,0,0,0,0"; str +=")";
        int res = calcIndex(idx,ModelConfig<Type>::getInstance()->dims);
        wtsPRINTVAR_DEBUG(str+" = ",res);    
        Type t = testArray_zsmxray(res);
        wtsPRINTVAR_DEBUG("t = ",t);
    }
    {
        idx << 0,1,0,0,0,0,0; 
        std::string str = "idx("; str +="0,1,0,0,0,0,0"; str +=")";
        int res = calcIndex(idx,ModelConfig<Type>::getInstance()->dims);
        wtsPRINTVAR_DEBUG(str+" = ",res);
        Type t = testArray_zsmxray(res);
        wtsPRINTVAR_DEBUG("t = ",t);
    }
    {
        int idy = pMC->dims[con::iY]-1;
        idx << 0,0,0,0,0,0,idy; 
        std::string str = "idx(";str+=+"0,0,0,0,0,0,"; str+=std::to_string(idy); str+=")";
        int res = calcIndex(idx,ModelConfig<Type>::getInstance()->dims);
        wtsPRINTVAR_DEBUG(str+" = ",res);
        Type t = testArray_zsmxray(res);
        wtsPRINTVAR_DEBUG("t = ",t);
    }

    if (debug>20) {
        wtsPRINTSTR_DEBUG("Printing testArray_zsmxra information");
        array<Type> testArray_zsmxra = testArray_zsmxray.col(0);
        wtsPRINTVAR_DEBUG("array dim = ",testArray_zsmxra.dim);
        wtsPRINTVAR_DEBUG("array mlt = ",testArray_zsmxra.mult);
        wtsPRINTVAR_DEBUG("ncols = ",testArray_zsmxra.cols());
        wtsPRINTVAR_DEBUG("nrows = ",testArray_zsmxra.rows());
    }

    //get parameter objects
    PARAMETER(pZ50);  //selectivity parameter vector
    if (debug>1) wtsPRINTVAR_DEBUG("pZ50 = ",pZ50)
    PARAMETER(pSlp);  //selectivity parameter vector
    if (debug>1) wtsPRINTVAR_DEBUG("pSlp = ",pSlp)

    //define objective function
    Type f = 0;

    //calculate selectivity curve
    double fZ = 100.0;
    vector<Type> p(2);
    p(0) = pZ50;
    p(1) = pSlp;
    vector<Type> s = asclogistic(stMC.zBs_z,p,fZ);

    //report s
    REPORT(s);

    //return objective function
    return f;
}
