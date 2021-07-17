/*
 * File:   tmbRecruitment.hpp
 * Author: WilliamStockhausen
 *
 * Created on October 7, 2019, 1:29 PM
 */

#ifndef RECRUITMENT_HPP
#define RECRUITMENT_HPP

#ifndef TMBTCSAM_HPP
  #include <TMB.hpp>
  #include <iostream>
  #include "utils.hpp"
  #include "ModelConfig.hpp"
#endif

template<class Type>
class Recruitment {
  public:
    /**
     * Constructor
     */
    Recruitment(){

    }

    Type getRec(int r, int y){
      Type r = exp(pLnR[iLnR(r,y)]+pDevs[iDevs(r,y)]];
      return r;
    }

    vector<Type> getRecSexRatio(int r,int y){
      Type expr = exp(pRX[iRX(r,y)]);
      Type p = expr/(1.0+expr);
      int nX = ModelConfig::dims(con::iX);
      vector<Type> f(nX);
      for (int x=0;x<nX;x++)
        f(x) = (x==con::X_MALE)*p + (x==con::X_FEMALE)*(1-p) + (x==con::X_UNDETERMINED);
      return f;
    }

    vector<Type> getRecSizeDist(int r, int y){
      vector<Type> zCs = pMC->zCs_z;
      vector<Type> R_zc = cgamma(zCs,pRa[iRa(r,y)],pRb[iRb(r,y)]);
      vector<Type> R_z = R_zc[upr]-R_zc[lwr];
      R_z /= R_z.sum();//normmalize to sum to 1 across size bins
      return R_z;
    }

    /**
     * Calculate array for recruitment dimensioned by zms.
     *
     * Recruitment is non-zero only for immature, new shell crab.
     *
     * @param Type R - total recruitment
     * @param vector<Type> sizeDist - size distribution for recruitment (normalized to sum to 1)
     *
     * @return 3d array<Type> dimensioned by zms
     */
    static array<Type> calcRec_zms(Type R, vector<Type> sizeDist){
      vector<int> dims_zsm(3);
      dims_zsm << ModelConfig::dims(con::iZ),
                  ModelConfig::dims(con::iS),
                  ModelConfig::dims(con::iM);

      array<Type> R_zsm(dims_zsm);//zeros out array
      (R_zsm.col(con::M_IMMATURE)).col(con::S_NEWSHELL) = R*sizeDist;
      return R_zsm;
    }

    /**
     * Calculate array for recruitment dimensioned by zmsxry.
     *
     * Recruitment is non-zero only for immature, new shell crab.
     *
     * @return 6d array<Type> dimensioned by zmsxry
     */
    static array<Type> calcRecruitment__zsmxry(){
      int iZ = ModelConfig::dims(con::iZ);
      int iS = ModelConfig::dims(con::iS);
      int iM = ModelConfig::dims(con::iM);
      int iX = ModelConfig::dims(con::iX);
      int iR = ModelConfig::dims(con::iR);
      int iA = ModelConfig::dims(con::iA);
      int iY = ModelConfig::dims(con::iY);
      vector<int> dims_zsmxry(6);
      dims_zsmxry << iZ,iS,iM,iX,iR,iA,iY;

      array<Type> R_zsmxry(dims_zsmxry);//zeros out array
      for (int y=0;y<iY;y++){
        for (int r=0;riR;r++){
          Type R = getRec(r,y);
          vector<Type> f_x  = getRecSexRatio(x,r,y);
          vector<Type> zD_z = getRecSizeDist(r,y);
          for (int x=0;x<iX;x++){
            R_zsmxry.col(x).col(r).col(y) = f_x(x)*calcRecruitment_zms(R,zD_z);
          }//-x
        }//-r
      }//-y
      return R_zsmxry;
    }

}//recruitment

#endif // RECRUITMENT_HPP //
