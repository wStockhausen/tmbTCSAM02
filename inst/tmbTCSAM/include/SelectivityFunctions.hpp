/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/* 
 * File:   SelectivityFunctions.hpp
 * Author: WilliamStockhausen
 *
 * Created on October 7, 2019, 1:21 PM
 */

#ifndef SELECTIVITYFUNCTIONS_HPP
#define SELECTIVITYFUNCTIONS_HPP

#include <TMB.hpp>
#include <iostream>
#include "constants.hpp"



/**
 * Calculates ascending logistic function parameterized by 
 *      params[1]: size at 50% selected (z50)
 *      params[2]: slope
 * Inputs:
 * @param z      - vector of sizes at which to compute function values
 * @param params - vector of function parameters
 * @param fsZ    - size at which function = 1 (i.e., fully-selected size) [double]
 * 
 * @return -
 */
template <class Type>
vector<Type> asclogistic(vector<Type> z, vector<Type> params, Type fsZ){
    if (DEBUG) Rcout<<"Starting asclogistic(...)"<<std::endl;
    Type n = (Type)1.0;
    vector<Type> s(z);
    s = 1.0/(1.0+exp(-params(1)*(z-params(0))));
    if (fsZ>0){
        n = 1.0+exp(-params(1)*(fsZ-params(0)));//normalize so (s(fsZ) = 1
        s *= n;
    } else if (fsZ<0) {
        n = 1.0/max(s);
        s *= n; //normalize by max
    } //otherwise don't normalize it
    if (DEBUG) {
        Rcout<<"params, fsZ = "<<params(1)<<con::tb<<params(2)<<con::tb<<fsZ<<std::endl;
        Rcout<<"n = "<<n<<std::endl;
        Rcout<<"z = "<<z<<std::endl;
        Rcout<<"s = "<<s<<std::endl;
        Rcout<<"Finished SelFcns::asclogistic(...)"<<std::endl;
    }
    return s;
}

/**
 * Calculates ascending logistic function parameterized by 
 *      params[1]: size at 50% selected (z50)
 *      params[2]: ln-scale increment from z50 to size at 95% selected
 * Inputs:
 * @param z      - vector of sizes at which to compute function values
 * @param params - vector of function parameters
 * @param fsZ    - size at which function = 1 (i.e., fully-selected size)
 * 
 * @return -
 */
template <class Type>
vector<Type> asclogistic50Ln95(vector<Type> z, vector<Type> params, Type fsZ){
    if (DEBUG) Rcout<<"Starting SelFcns::asclogistic50Ln95(...)"<<std::endl;
    Type n;
    vector<Type> s(z);
    Type z50    = params(0);
    Type dz5095 = exp(params(1));
    s = 1.0/(1.0+exp(-log(19.0)*(z-z50)/dz5095));
    if (fsZ>0){
        n = 1.0+exp(-log(19.0)*(fsZ-z50)/dz5095);//normalization constant
        s *= n;
    } else if (fsZ<0) {
        n = 1.0/max(s);
        s *= n; //normalize by max
    } //otherwise don't normalize it
    if (DEBUG) {
        Rcout<<"params, fsZ = "<<params(0)<<con::tb<<params(1)<<con::tb<<fsZ<<std::endl;
        Rcout<<"n = "<<n<<std::endl;
        Rcout<<"z = "<<z<<std::endl;
        Rcout<<"s = "<<s<<std::endl;
        Rcout<<"Finished SelFcns::asclogistic50Ln95(...)"<<std::endl;
    }
    return s;
}

/**
 * Calculates ascending normal function parameterized by 
 *      params[1]: size at which ascending limb reaches 1
 *      params[2]: width of ascending limb
 * Inputs:
 * @param z      - vector of sizes at which to compute function values
 * @param params - vector of function parameters
 * @param fsZ    - size at which function = 1 (i.e., fully-selected size) NOTE: ignored!
 * 
 * @return - selectivity function values as vector
 */
template <class Type>
vector<Type> ascnormal(vector<Type> z, vector<Type> params, Type fsZ){
    if (DEBUG) Rcout<<"Starting ascnormal(...)"<<std::endl;
    vector<Type> ascN(z);
    vector<Type> ascJ(z);
    Type slp = 5.0;
    Type ascMnZ = params(1-1);//size at which ascending limb hits 1
    Type ascWdZ = params(2-1);//width of ascending limb
    ascN = exp(-0.5*square((z-ascMnZ)/ascWdZ));
    ascJ = 1.0/(1.0+exp(slp*(z-(ascMnZ))));
    vector<Type> s = ascJ*ascN+(1.0-ascJ);
    if (DEBUG) Rcout<<"Finished ascnormal(...)"<<std::endl;
    return s;
}

/**
 * Calculates 4-parameter normal function parameterized by 
 *      params[1]: size at which ascending limb reaches 1
 *      params[2]: width of ascending limb
 *      params[3]: size at which descending limb departs from 1
 *      params[4]: width of descending limb
 * Inputs:
 * @param z      - vector of sizes at which to compute function values
 * @param params - vector of function parameters
 * @param fsZ    - size at which function = 1 (i.e., fully-selected size) NOTE: ignored!
 * 
 * @return - selectivity function values as a vector
 */
template <class Type>
vector<Type> dblnormal4(vector<Type> z, vector<Type> params, Type fsZ){
    if (DEBUG) Rcout<<"Starting dblnormal4(...)"<<std::endl;
    Type slp = 5.0;
    Type ascMnZ = params(1-1);//size at which ascending limb hits 1
    Type ascWdZ = params(2-1);//width of ascending limb
    Type dscMnZ = params(3-1);//size at which descending limb departs from 1
    Type dscWdZ = params(4-1);//width of descending limb
    vector<Type> ascN = exp(-0.5*square((z-ascMnZ)/ascWdZ));
    vector<Type> ascJ = 1.0/(1.0+exp(slp*(z-(ascMnZ))));
    vector<Type> dscN = exp(-0.5*square((z-dscMnZ)/dscWdZ));
    vector<Type> dscJ = 1.0/(1.0+exp(-slp*(z-(dscMnZ))));
    vector<Type> s = ((ascJ*ascN)+(1.0-ascJ))*((dscJ*dscN)+(1.0-dscJ));
    if (DEBUG) Rcout<<"Finished dblnormal4(...)"<<std::endl;
    return s;
}

/**
 * Calculates 6-parameter normal function parameterized by 
 *      params[1]: size at which ascending limb reaches 1
 *      params[2]: width of ascending limb
 *      params[3]: size at which descending limb departs from 1
 *      params[4]: width of descending limb
 *      params[5]: floor of ascending limb
 *      params[6]: floor of descending limb
 * Inputs:
 * @param z      - vector of sizes at which to compute function values
 * @param params - vector of function parameters
 * @param fsZ    - size at which function = 1 (i.e., fully-selected size) NOTE: ignored!
 * 
 * @return - selectivity function values as dvar_vector
 */
template <class Type>
vector<Type> dblnormal6(vector<Type> z, vector<Type> params, Type fsZ){
    if (DEBUG) Rcout<<"Starting dblnormal6(...)"<<std::endl;
    Type slp = 5.0;
    Type ascMnZ = params(1-1);//size at which ascending limb hits 1
    Type ascWdZ = params(2-1);//width of ascending limb
    Type dscMnZ = params(3-1);//size at which descending limb departs from 1
    Type dscWdZ = params(4-1);//width of descending limb
    Type ascFlr = params(5-1);//floor of ascending limb
    Type dscFlr = params(6-1);//floor of descending limb
    vector<Type> ascN = ascFlr+(1.0-ascFlr)*exp(-0.5*square((z-ascMnZ)/ascWdZ));
    vector<Type> ascJ = 1.0/(1.0+exp(slp*(z-(ascMnZ))));
    vector<Type> dscN = dscFlr+(1.0-dscFlr)*exp(-0.5*square((z-dscMnZ)/dscWdZ));
    vector<Type> dscJ = 1.0/(1.0+exp(-slp*(z-(dscMnZ))));
    vector<Type> s = ((ascJ*ascN)+(1.0-ascJ))*((dscJ*dscN)+(1.0-dscJ));
    if (DEBUG) Rcout<<"Finished dblnormal6(...)"<<std::endl;
    return s;
}

/**
 * Calculates "constant" selectivity function (=1 at all sizes)
 * Inputs:
 * @param z - vector of sizes at which to compute function values
 * 
 * @return - selectivity function values as vector
 */
template <class Type>
vector<Type> constant(vector<Type> z){
    if (DEBUG) Rcout<<"Starting constant(...)"<<std::endl;
    vector<Type> s(z);
    s = 1.0;
    if (DEBUG) Rcout<<"Finished constant(...)"<<std::endl;
    return s;
}       

/**
 * Calculates "nonparametric" selectivity function with smoothness imposed
 * on the resulting curve by way of penalties in the objective function.
 * Inputs:
 * @param z      - dvector of sizes at which to compute function values
 * @param params - vector<Type> of function parameters, 1 for each size bin
 * @param idZ    - index at which function = 1 (i.e., fully-selected size) [int]
 * 
 * @return - selectivity function values as dvar_vector
 */
template <class Type>
vector<Type> nonparametric(vector<Type> z, vector<Type> params, int idZ){
    if (DEBUG) Rcout<<"Starting nonparametric(...)"<<std::endl;
    if (DEBUG) Rcout<<"z size = "<<z.size()<<std::endl;
    if (DEBUG) Rcout<<"p size = "<<params.size()<<std::endl;
    vector<Type> s = 1.0/(1.0+exp(-params));//unnormalized
    if (idZ>0) s *= (1.0+exp(-params[idZ]));//normalized to 1
    if (params.indexmin()>z.indexmin()) s(z.indexmin(),params.indexmin()-1) = 0.0;//set lower to 0
    if (params.indexmax()<z.indexmax()) s(params.indexmax()+1,z.indexmax()) = 1.0;//set upper to 1
    if (DEBUG) Rcout<<"Finished nonparametric(...)"<<std::endl;
    return s;
}       
#endif /* SELECTIVITYFUNCTIONS_HPP */

