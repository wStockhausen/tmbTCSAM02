/*
 * File:   utils.hpp
 * Author: WilliamStockhausen
 *
 * Created on October 7, 2019, 1:36 PM
 */

#ifndef UTILS_HPP
#define UTILS_HPP

#ifndef TMBTCSAM_HPP
    #include <TMB.hpp>
    #include <iostream>
#endif

namespace con {
  const int iZ = 0;//position index for size bins
  const int iS = 1;//position index for shell conditions
  const int iM = 2;//position index for maturity states
  const int iX = 3;//position index for sexes
  const int iR = 4;//position index for regions
  const int iA = 5;//position index for ages
  const int iY = 6;//position index for years

  //shell conditions
  const int S_NEWSHELL     = 0;
  const int S_OLDSHELL     = 1;
  const int S_UNDETERMINED = 2;
  //maturity states
  const int M_IMMATURE     = 0;
  const int M_MATURE       = 1;
  const int M_UNDETERMINED = 2;
  //sexes
  const int X_MALE         = 0;
  const int X_FEMALE       = 1;
  const int X_UNDETERMINED = 2;

  //character strings for output
  const char tb[] = "\t";//tab character
  const char cc[] = ", ";//comma character with space
}//con namespace

//macro to print debugging info from a text string
#ifdef wtsPRINTSTR_DEBUG
        #undef wtsPRINTSTR_DEBUG
#endif
#define wtsPRINTSTR_DEBUG(t) if (debug) {std::cout<<(t)<<std::endl;}

//macro to print debugging info for a variable
#ifdef wtsPRINTVAR_DEBUG
        #undef wtsPRINTVAR_DEBUG
#endif
#define wtsPRINTVAR_DEBUG(t,v) if (debug) {std::cout<<(t)<<std::endl<<con::tb; wts::print(v); std::cout<<std::endl;}

//macro to print debugging info for a struture with a print function
#ifdef wtsPRINTSTRUCT_DEBUG
        #undef wtsPRINTSTRUCT_DEBUG
#endif
#define wtsPRINTSTRUCT_DEBUG(t,s) if (debug) {std::cout<<(t)<<std::endl; s.print(std::cout); std::cout<<std::endl;}

namespace wts{
    /**
    * Print a vector.
    *
    * Print a vector. Individual elements should be printable.
    *
    * @param vector<Type> v - vector to print
    * @param char* sep - string to use as separator between vector elements
    */
    template<class Type>
    void print(vector<Type> v,const char* sep=con::tb){
            int n = v.size();
            for (int i=0;i<(n-1);i++) std::cout<<v[i]<<sep;
            std::cout<<v[n-1];
    }

        template<class Type>
        void print(Type v){
                std::cout<<v;
        }
} //namespace wts
#endif /* UTILS_HPP */

