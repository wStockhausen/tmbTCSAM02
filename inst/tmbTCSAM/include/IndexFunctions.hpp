/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */

/*
 * File:   IndexFunctions.hpp
 * Author: WilliamStockhausen
 *
 * Created on November 9, 2019, 11:06 AM
 */

#ifndef INDEXFUNCTIONS_HPP
#define INDEXFUNCTIONS_HPP

#ifndef TMBTCSAM_HPP
  #include <TMB.hpp>
  #include <iostream>
  #include "utils.hpp"
#endif

#define DEBUG_INDEXFUNCTIONS
#ifdef DEBUG_INDEXFUNCTIONS
    #undef DEBUG_INDEXFUNCTIONS
#endif
#undef DEBUG_INDEXFUNCTIONS

/**
 * Calculate product of a vector.
 *
 * Calculates the product of a vector.
 *
 * @param v - vector 
 * @return the corresponding product
 */
template <class Type>
Type calcProd(vector<Type>& v){
    int n = v.size();//size of vector
#ifdef DEBUG_INDEXFUNCTIONS
        std::cout << "n = " << n << std::endl;
        std::cout << "----v = "; wts::print(v); std::cout << std::endl;
#endif
    Type p = v(0);
    for (int j=1;j<n;j++) p *= v(j);
#ifdef DEBUG_INDEXFUNCTIONS 
    std::cout << "----p = " << p << std::endl;
#endif
    return p;
}

/**
 * Calculate a 1-d index value for a multidimensional array.
 *
 * Calculates the 1-d index value for a multidimensional array
 * corresponding to a vector of 0-based index values and dimensions.
 *
 * By default this calculation assumes the array is a
 * column major (non-ragged) array.
 *
 * @param iv - vector with 0-based multidimensional indices
 * @param dims - vector with dimensions for the indices
 * @param isCM - flag indicating whether indices are "column major" or "row major"
 * @return the corresponding 1-d index
 */
int calcIndex(vector<int>& iv, vector<int>& dims, bool isCM=true){
    int n = iv.size();//number of indices (how get size of vector?)
    int i = 0;
#ifdef DEBUG_INDEXFUNCTIONS 
    std::cout << "n    = " << n << std::endl;
    std::cout << "iv   = "; wts::print(iv);   std::cout << std::endl;
    std::cout << "dims = "; wts::print(dims); std::cout << std::endl;
#endif
    if (isCM) {
        i = iv(0);
#ifdef DEBUG_INDEXFUNCTIONS 
        std::cout<<i<<" "<<std::endl;
#endif
        for (int j=1;j<n;j++) {
            vector<int> dh = dims.head(j);//need to use implicit cast here from Eigen::VectorBlock
            int prod = calcProd(dh);
            i += iv(j)*prod;
#ifdef DEBUG_INDEXFUNCTIONS 
            std::cout << "j = " << j << ", iv(j) = " << iv(j) << ", prod(j) = " << prod << std::endl;
            std::cout << "i = " << i << std::endl;
#endif
        }
    }
    return i;
}

#endif /* INDEXFUNCTIONS_HPP */

