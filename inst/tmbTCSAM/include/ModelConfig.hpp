/*
 * File:   ModelConfig.hpp
 * Author: WilliamStockhausen
 *
 * Created on December 10, 2019.
 */

#ifndef MODELCONFIG_HPP
#define MODELCONFIG_HPP

#ifndef TMBTCSAM_HPP
  #include <TMB.hpp>
  #include <iostream>
  #include "utils.hpp"
#endif

template <class Type>  //needs to be defined as a template for DATA_STRUCT macro
struct structModelConfig{
    std::string scenario;
    vector<int> dims;//model dimensions: z,s,m,x,r,a,y
    int mnYr;//min model year
    int mxYr;//max model year
    int nFsh;//number of fisheries
    int nSrv;//number of surveys
    vector<int> yrs_y;  //model years
    vector<Type> zBs_z; //model size bins
    vector<Type> zCs_z; //model size bin cutpoints

    structModelConfig(SEXP mc){
      scenario = CHAR(STRING_ELT(getListElement(mc,"scenario"),0));
      dims  = asVector<int>(getListElement(mc,"dims"));
      mnYr  = asVector<int>(getListElement(mc,"mnYr"))[0];
      mxYr  = asVector<int>(getListElement(mc,"mxYr"))[0];
      nFsh  = asVector<int>(getListElement(mc,"nFsh"))[0];
      nSrv  = asVector<int>(getListElement(mc,"nSrv"))[0];
      yrs_y = asVector<int>(getListElement(mc,"yrs_y"));
      zBs_z = asVector<Type>(getListElement(mc,"zBs_z"));
      zCs_z = asVector<Type>(getListElement(mc,"zCs_z"));
    }

    void print(std::ostream& os){
            os<<"\tscenario = "<<scenario<<std::endl;
            os<<"\tnY = "<<dims(con::iY)<<std::endl;
            os<<"\tnA = "<<dims(con::iA)<<std::endl;
            os<<"\tnR = "<<dims(con::iR)<<std::endl;
            os<<"\tnX = "<<dims(con::iX)<<std::endl;
            os<<"\tnM = "<<dims(con::iM)<<std::endl;
            os<<"\tnS = "<<dims(con::iS)<<std::endl;
            os<<"\tnZ = "<<dims(con::iZ)<<std::endl;
            os<<"\tmnYr = "<<mnYr<<std::endl;
            os<<"\tmxYr = "<<mxYr<<std::endl;
            os<<"\tnFsh = "<<nFsh<<std::endl;
            os<<"\tnSrv = "<<nSrv<<std::endl;
            os<<"\tyrs_y  = "; wts::print(yrs_y); os<<std::endl;
            os<<"\tzBs_z  = "; wts::print(zBs_z); os<<std::endl;
            os<<"\tzCs_z  = "; wts::print(zCs_z); os<<std::endl;
    }
};

template <class Type>
class ModelConfig {
  public:
    /**
     * Static method to create and get "global" ModelConfig instance based on a structModelConfig object
     *
     * @param const structModelConfig& mc - reference to a structModelConfig object
     */
    static ModelConfig* getInstance(const structModelConfig<Type>& mc){
      if (!pMC) delete pMC;
      pMC = new ModelConfig(mc);
      return pMC;
    }

    /**
     * Static method to get the (already created) "global" instance
     */
    static ModelConfig<Type>* getInstance(void){
      return pMC;
    }

  private:
    static ModelConfig<Type>* pMC;

  public:
    std::string scenario;
    vector<int> dims;//model dimensions: z,s,m,x,r,a,y
    int mnYr;//min model year
    int mxYr;//max model year
    int nFsh;//number of fisheries
    int nSrv;//number of surveys
    vector<int> yrs_y;  //model years
    vector<Type> zBs_z; //model size bins
    vector<Type> zCs_z; //model size bin cutpoints

  protected:
    /**
     * Constructor
     *
     * @param SEXP mc - pointer to model configuration list from R
     */
    ModelConfig(const structModelConfig<Type>& mc){
      scenario = mc.scenario;
      dims  = mc.dims;
      mnYr  = mc.mnYr;
      mxYr  = mc.mxYr;
      nFsh  = mc.nFsh;
      nSrv  = mc.nSrv;
      yrs_y = mc.yrs_y;
      zBs_z = mc.zBs_z;
      zCs_z = mc.zCs_z;
    }

    /**
     * Destructor.
     */
    ~ModelConfig<Type>(){}

  public:
    /**
     * Print method
     *
     * @param std::ostream& os - output stream to print to
     */
    void print(std::ostream& os){
            os<<"\tscenario = "<<scenario<<std::endl;
            os<<"\tnY = "<<dims(con::iY)<<std::endl;
            os<<"\tnA = "<<dims(con::iA)<<std::endl;
            os<<"\tnR = "<<dims(con::iR)<<std::endl;
            os<<"\tnX = "<<dims(con::iX)<<std::endl;
            os<<"\tnM = "<<dims(con::iM)<<std::endl;
            os<<"\tnS = "<<dims(con::iS)<<std::endl;
            os<<"\tnZ = "<<dims(con::iZ)<<std::endl;
            os<<"\tmnYr = "<<mnYr<<std::endl;
            os<<"\tmxYr = "<<mxYr<<std::endl;
            os<<"\tnFsh = "<<nFsh<<std::endl;
            os<<"\tnSrv = "<<nSrv<<std::endl;
            os<<"\tyrs_y  = "; wts::print(yrs_y); os<<std::endl;
            os<<"\tzBs_z  = "; wts::print(zBs_z); os<<std::endl;
            os<<"\tzCs_z  = "; wts::print(zCs_z); os<<std::endl;
    }

    friend std::ostream& operator <<(std::ostream & os, ModelConfig<Type>& obj){obj.print(os); return os;}
};

template <class Type> ModelConfig<Type>* ModelConfig<Type>::pMC = NULL;//set static variable to NULL

#endif /* MODELCONFIG_HPP */

