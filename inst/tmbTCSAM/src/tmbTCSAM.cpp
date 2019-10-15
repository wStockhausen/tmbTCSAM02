#include <TMB.hpp>
const int DEBUG = 0;
#include "../include/tmbTCSAM.hpp"

template<class Type>
Type objective_function<Type>::operator() () {
    DATA_VECTOR(zBs_z);   //size bin mid points
    PARAMETER(pZ50);  //selectivity parameter vector
    PARAMETER(pSlp);  //selectivity parameter vector

    //define objective function
    Type f = 0;

    //calculate selectivity curve
    double fZ = 100.0;
    vector<Type> p(2);
    p(0) = pZ50;
    p(1) = pSlp;
    vector<Type> s = asclogistic(zBs_z,p,fZ);

    //report s
    REPORT(s);

    //return objective function
    return f;
}
