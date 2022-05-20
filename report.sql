/* Formatted on 20/05/2022 11:16:01 am (QP5 v5.115.810.9015) */
SELECT DCategorys,                                                       --DGL
       SUBSTR (DGL, 1, 8) || ' USD' DGL,
       DDescription,
       DBalance DBalance
FROM (SELECT 'DEPOSIT' AS DCategorys,
             ACLSS.CR_GL || ' ' || BAL.CCY AS DGL,
             GLM.GL_DESC AS DDescription,
             SUM (BAL.AC_CLASS_BALANCE) AS DBalance,
             (CASE
                 WHEN :PM_FUND_ID = 'FCYPOOL'
                 THEN
                    (SELECT MAX (H.MID_RATE)
                     FROM flexcube.CYTB_RATES_HISTORY H
                     WHERE H.RATE_DATE =
                              (SELECT MAX (E.RATE_DATE)
                               FROM flexcube.CYTB_RATES_HISTORY E
                               WHERE     H.BRANCH_CODE = E.BRANCH_CODE
                                     AND H.CCY1 = E.CCY1
                                     AND H.CCY2 = E.CCY2
                                     AND E.RATE_TYPE = 'STANDARD'
                                     AND E.RATE_DATE <=
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                           AND H.CCY1 = :CCY
                           AND H.CCY2 = 'PKR'
                           AND H.RATE_TYPE = 'STANDARD'
                           AND H.BRANCH_CODE = '702')
              END)
                RATE
      FROM flexcube.iatm_liability_statistics Bal,            ---_hist removed
           flexcube.sttm_account_class_status ACLSS,
           flexcube.gltm_glmaster GLM,
           flexcube.sttm_branch B
      WHERE     BAL.FUND_ID = :PM_FUND_ID
            AND BAL.BRANCH_CODE = B.BRANCH_CODE
            AND BAL.ACCOUNT_CLASS = ACLSS.ACCOUNT_CLASS
            AND ACLSS.CR_GL = GLM.GL_CODE
            AND ACLSS.STATUS = 'NORM'
            AND POOL_BAL_TYPE = 'D'
            AND bal.CALC_DATE =
                  (SELECT MAX (M2.CALC_DATE)
                   FROM flexcube.iatm_liability_statistics M2
                   WHERE     M2.ACCOUNT_CLASS = Bal.ACCOUNT_CLASS
                         AND M2.BRANCH_CODE = Bal.BRANCH_CODE
                         AND M2.CCY = Bal.CCY
                         AND M2.FUND_ID = :PM_FUND_ID
                         AND M2.CALC_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
            AND bal.BRANCH_CODE IN (SELECT BRANCH_CODE
                                    FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                    WHERE IS_LIVE = 'Y')
      GROUP BY ACLSS.CR_GL, GLM.GL_DESC, BAL.CCY);

 /*union all 
  
--- added 22-08-17  migration GLS 
 SELECT --- GL||' '|| CCY AS GL, GL_DESC, CCY,DEPOSITS_TRIAL_BALANCE,DEPOSITS_ALL_BALANCE,DIFF 
 'DEPOSIT' AS DCategorys,gls.GL_CODE||' '|| CURRENCY AS DGL ,GL_CODE_DESCRIPTION AS DDescription,flx_gl_bal AS DBalance 
-- gls.GL_CODE||' '|| CURRENCY, GL_CODE_DESCRIPTION GL_DESC,currency 
 --, flx_gl_bal DEPOSITS_TRIAL_BALANCE, nvl(ConsBalance,0) DEPOSITS_ALL_BALANCE,nvl(flx_gl_bal,0)-nvl(ConsBalance,0) DIFF 
  FROM   flexcube.IATM_CATEGORY_GL_FUND_LINK   GLS 
  left join   --- trail bal 
  
  ( 
  select GL_CODE,CCY_CODE,sum(-1*DR_BAL_LCY)+sum(CR_BAL_LCY) flx_gl_bal 
from flexcube.GLTB_GL_BAL 
where (fin_year='FY2017' AND PERIOD_CODE='M06' and GL_CODE LIKE '32%') 
and Branch_code in (SELECT BRANCH_CODE FROM  flexcube.STTM_BRANCH_ISLAMIC_CUSTOM   WHERE IS_LIVE = 'Y') 
group by GL_CODE,CCY_CODE 
)  flxgl 
on flxgl.gl_code=GLS.gl_code and flxgl.CCY_CODE=currency 
  
   WHERE       
    
    GLS.CATEGORY_TYPE = 'LIABILITY' 
           AND GLS.RECORD_STAT = 'O' AND GLS.AUTH_STAT = 'A' 
AND gls.GL_CODE LIKE '32%'---  migration gls 

*/
-------------------------------------------

/*  old querry 
  SELECT   'DEPOSIT' AS DCategorys, 
           ACLSS.CR_GL ||' '||  BAL.CCY AS DGL , 
            
           GLM.GL_DESC AS DDescription,sum(BAL.AC_CLASS_BALANCE )AS DBalance 
--           SUM (CYPKS_ADDON.fn_conv_amt1_to_amt2 (BAL.BRANCH_CODE, 
--                                                  BAL.CCY, 
--                                                  B.BRANCH_LCY, 
--                                                  'STANDARD', 
--                                                  'M', 
--                                                  BAL.AC_CLASS_BALANCE, 
--                                                  'N')) 
--              AS DBalance 
    FROM      flexcube.iatm_liability_statistics_HIST  Bal 
           INNER JOIN 
              (  SELECT   BRANCH_CODE, MAX (EOD_DATE) EOD_DATE 
                   FROM   (SELECT   BRANCH_CODE, EOD_DATE 
                             FROM   flexcube.AETB_EOC_BRANCHES 
                            WHERE   EOC_STATUS = 'C' 
                           UNION 
                           SELECT   BRANCH_CODE, EOD_DATE 
                             FROM    flexcube.AETB_EOC_BRANCHES_HISTORY 
                            WHERE   EOC_STATUS = 'C') 
                  WHERE   EOD_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD') 
               GROUP BY   BRANCH_CODE) EOD 
           ON (EOD.BRANCH_CODE = Bal.BRANCH_CODE 
               AND EOD.EOD_DATE = BAL.CALC_DATE), 
            flexcube.sttm_account_class_status ACLSS, 
            flexcube.gltm_glmaster GLM, 
            flexcube.sttm_branch B 
   WHERE       BAL.FUND_ID = :PM_FUND_ID 
           AND BAL.BRANCH_CODE = B.BRANCH_CODE 
           AND BAL.ACCOUNT_CLASS = ACLSS.ACCOUNT_CLASS 
           AND ACLSS.CR_GL = GLM.GL_CODE 
           AND ACLSS.STATUS = 'NORM' 
          and POOL_BAL_TYPE ='D' 
GROUP BY   ACLSS.CR_GL, GLM.GL_DESC, BAL.CCY 
Formatted on 3/25/2014 3:10:59 PM (QP5 v5.115.810.9015) */


--current

SELECT CCategorys,
       CGL,
       CDescription,
       CBalance
FROM (SELECT 'CURRENT ACCOUNT' AS CCategorys,
             STAT.DR_GL || ' ' || BASE_CCY AS CGL,
             ACCLS.DESCRIPTION AS CDescription,
             SUM (EQU.LCY_CONTRIBUTION_AMOUNT) AS CBalance,
             (CASE
                 WHEN :PM_FUND_ID = 'FCYPOOL'
                 THEN
                    (SELECT MAX (H.MID_RATE)
                     FROM flexcube.CYTB_RATES_HISTORY H
                     WHERE H.RATE_DATE =
                              (SELECT MAX (E.RATE_DATE)
                               FROM flexcube.CYTB_RATES_HISTORY E
                               WHERE     H.BRANCH_CODE = E.BRANCH_CODE
                                     AND H.CCY1 = E.CCY1
                                     AND H.CCY2 = E.CCY2
                                     AND E.RATE_TYPE = 'STANDARD'
                                     AND E.RATE_DATE <=
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                           AND H.CCY1 = :CCY
                           AND H.CCY2 = 'PKR'
                           AND H.RATE_TYPE = 'STANDARD'
                           AND H.BRANCH_CODE = '702')
              END)
                RATE
      FROM flexcube.IATM_EQUITY_CONTRIBUTION EQU,
           --IATM_EQUITY_CONTRIB_HIST removed
           flexcube.STTM_ACCOUNT_CLASS_STATUS STAT,
           flexcube.STTM_ACCOUNT_CLASS ACCLS,
           flexcube.gltm_glmaster GLM,
           flexcube.AMTM_FUND_MASTER pool
      WHERE     EQU.FUND_ID = :PM_FUND_ID
            AND STAT.ACCOUNT_CLASS = EQU.ACCOUNT_CLASS
            AND STAT.STATUS = 'NORM'
            AND STAT.DR_GL = GLM.GL_CODE
            AND pool.FUND_ID = equ.FUND_ID
            AND EQU.RUN_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
            /*(SELECT   MAX (BAL2.RUN_DATE)
               FROM    flexcube.IATM_POOL_BAL_EXECUTION  BAL2
              WHERE   BAL2.RUN_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD') and POOL_BAL_TYPE ='D')*/


            AND EQU.ACCOUNT_CLASS <> 'EQUITY'
            AND EQU.ACCOUNT_CLASS = ACCLS.ACCOUNT_CLASS
            AND POOL_BAL_TYPE = 'D'
      GROUP BY STAT.DR_GL, ACCLS.DESCRIPTION, BASE_CCY);


---fixed

SELECT NVL (SUM (D.ALLOCATED_AMOUNT), 0) AS Balance
FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M1,
     flexcube.IATM_ASSET_FUND_LINK D,
     flexcube.CLTB_ACCOUNT_APPS_MASTER CL,
     flexcube.CLTM_PRODUCT PRD
WHERE M1.EFF_DATE =
         (SELECT MAX (M2.EFF_DATE)
          FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M2
          WHERE     M2.ASSET_CODE = M1.ASSET_CODE
                AND M2.ASSET_CCY = M1.ASSET_CCY
                AND M2.SUKKUKH_CODE = M1.SUKKUKH_CODE
                AND M2.EFF_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
      AND D.FUND_ID = :PM_FUND_ID
      AND M1.ASSET_CODE = D.ASSET_CODE
      AND M1.ASSET_CCY = D.ASSET_CCY
      AND M1.SUKKUKH_CODE = D.SUKKUKH_CODE
      AND D.ASSET_CODE = CL.ACCOUNT_NUMBER
      AND CL.PRODUCT_CODE = PRD.PRODUCT_CODE
      AND PRD.FA_PRODUCT IS NOT NULL;


---New DataSet 7

SELECT :PM_FUND_ID,
       TO_CHAR (
          TO_DATE (
                SUBSTR (:PM_DATE, 7, 2)
             || '-'
             || SUBSTR (:PM_DATE, 5, 2)
             || '-'
             || SUBSTR (:PM_DATE, 1, 4),
             'dd-MM-yyyy'
          ),
          'DD-Mon-YYYY'
       )
          DT,
       TO_CHAR ( (TO_DATE (FUND_START_DATE, 'DD-MON-yy')), 'DD-Mon-YYYY')
          S_DATE
FROM flexcube.AMTM_FUND_MASTER
WHERE FUND_ID = :PM_FUND_ID;

--select   :PM_FUND_ID,to_char(to_date(substr(:PM_DATE,7,2)||'-'||substr(:PM_DATE,5,2)||'-'||substr(:PM_DATE,1,4),'dd-MM-yyyy'),'DD-Mon-YYYY') --------- DT,to_char(FUND_START_DATE,'DD-Mon-YYYY') FUND_START_DATE  from AMTM_FUND_MASTER where FUND_ID=:PM_FUND_ID

--Equity All Poll taken

SELECT ECategorys, E_All_BAll_Balance * NVL (RATE, 1) E_All_BAll_Balance
FROM (SELECT 'EQUITY ALL' || BASE_CCY AS ECategorys,
             NVL (SUM (EQU.LCY_CONTRIBUTION_AMOUNT), 0) AS E_All_BAll_Balance,
             (CASE
                 WHEN :PM_FUND_ID = 'FCYPOOL'
                 THEN
                    (SELECT MAX (H.MID_RATE)
                     FROM flexcube.CYTB_RATES_HISTORY H
                     WHERE H.RATE_DATE =
                              (SELECT MAX (E.RATE_DATE)
                               FROM flexcube.CYTB_RATES_HISTORY E
                               WHERE     H.BRANCH_CODE = E.BRANCH_CODE
                                     AND H.CCY1 = E.CCY1
                                     AND H.CCY2 = E.CCY2
                                     AND E.RATE_TYPE = 'STANDARD'
                                     AND E.RATE_DATE <=
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                           AND H.CCY1 = :CCY
                           AND H.CCY2 = 'PKR'
                           AND H.RATE_TYPE = 'STANDARD'
                           AND H.BRANCH_CODE = '702')
              END)
                RATE
      FROM Flexcube.IATM_EQUITY_CONTRIBuTION EQU,
           flexcube.AMTM_FUND_MASTER pool
      WHERE     equ.fund_id = :PM_FUND_ID
            --           EQU.FUND_ID IN (select distinct fund_id from flexcube.IATM_POOL_BAL_ORDER
            --                                          where fund_id not in (select FUND_ID from flexcube.IATM_POOL_BAL_ORDER  where POOL_BAL_TYPE  ='D' and
            --                                           run_date =   TO_DATE (:PM_DATE, 'YYYY-MM-DD') ))
            AND EQU.RUN_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
            /* (SELECT   MAX (BAL2.RUN_DATE)
                FROM  flexcube.IATM_POOL_BAL_EXECUTION  BAL2
               WHERE   BAL2.RUN_DATE <=   TO_DATE (:PM_DATE, 'YYYY-MM-DD'))*/

            AND EQU.ACCOUNT_CLASS = 'EQUITY'
            AND equ.POOL_BAL_TYPE = 'D'
            AND pool.FUND_ID = equ.FUND_ID
      GROUP BY BASE_CCY);

--Ass_contracts

SELECT ACategorys,
       --   AGL,
       CASE WHEN AGL <> 'ALL' THEN SUBSTR (AGL, 1, 8) || ' USD' END AGL,
       ACC_CCY,
       ADescription,
       ABalance + NVL (ALL_BALANCE_Place, 0) ABalance
FROM    (SELECT a1.PRODUCT_CATEGORY AS ACategorys,
                p1.ACCOUNT_HEAD || ' ' || d1.ASSET_CCY AS AGL,
                d1.ASSET_CCY ACC_CCY,
                G1.GL_DESC AS ADescription,
                --- changed on 06-JAn-2019 on adeel's request , coversion to be done on report end only for assets/contracts, in presence of MR muddasar

                SUM (D1.ALLOCATED_AMOUNT)
                * (CASE
                      WHEN :PM_FUND_ID = 'FCYPOOL1'
                      THEN
                         (SELECT MAX (H.MID_RATE)
                          FROM flexcube.CYTB_RATES_HISTORY H
                          WHERE H.RATE_DATE =
                                   (SELECT MAX (E.RATE_DATE)
                                    FROM flexcube.CYTB_RATES_HISTORY E
                                    WHERE     H.BRANCH_CODE = E.BRANCH_CODE
                                          AND H.CCY1 = E.CCY1
                                          AND H.CCY2 = E.CCY2
                                          AND E.RATE_TYPE = 'STANDARD'
                                          AND E.RATE_DATE <=
                                                TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                                AND H.CCY1 = :CCY
                                AND H.CCY2 = 'PKR'
                                AND H.RATE_TYPE = 'STANDARD'
                                AND H.BRANCH_CODE = '702')
                      ELSE
                         1
                   END)
                   AS ABalance
         FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M1,
              flexcube.IATM_ASSET_FUND_LINK D1,
              flexcube.CLTB_ACCOUNT_APPS_MASTER A1,
              flexcube.cltm_product_rth P1,
              flexcube.GLTM_GLMASTER G1
         WHERE M1.EFF_DATE =
                  (SELECT MAX (M2.eff_date)
                   FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M2
                   WHERE     M2.EFF_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                         AND M2.ASSET_CODE = M1.ASSET_CODE
                         AND M2.SUKKUKH_CODE = ' '
                         AND M2.ASSET_CCY = M1.ASSET_CCY
                         AND M2.AUTH_STAT = 'A'
                         AND M2.RECORD_STAT = 'O')
               AND M1.EFF_DATE = D1.EFF_DATE
               -- AND a1.USER_DEFINED_STATUS = 'NORM'
               AND M1.ASSET_CODE = D1.ASSET_CODE
               AND D1.FUND_ID = :PM_FUND_ID
               --  and M1.asset_code in(:CCY)
               AND M1.SUKKUKH_CODE = D1.SUKKUKH_CODE
               AND M1.ASSET_CCY = D1.ASSET_CCY
               AND M1.AUTH_STAT = 'A'
               AND M1.RECORD_STAT = 'O'
               AND M1.ASSET_CODE = A1.ACCOUNT_NUMBER
               AND A1.PRODUCT_CODE = P1.PRODUCT_CODE
               AND P1.ACCOUNTING_ROLE = 'LOAN_ACCOUNT'
               AND G1.GL_CODE = P1.ACCOUNT_HEAD
               AND A1.Branch_code IN (SELECT BRANCH_CODE
                                      FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                      WHERE IS_LIVE = 'Y')
         --in (select to_char(OFF_CODE) from ask_uni.branches_area where flexcube='I')
         GROUP BY A1.PRODUCT_CATEGORY,
                  P1.ACCOUNT_HEAD,
                  G1.GL_DESC,
                  D1.ASSET_CCY) MA
     LEFT JOIN
        (SELECT GLS.GL_CODE AS GL,
                GLS.GL_CODE_DESCRIPTION AS GL_DESC,        --BAL.AC_CCY   CCY,
                CASE
                   WHEN bal.FUND_ID = 'FCYPOOL' THEN 'PKR'
                   ELSE BAL.AC_CCY
                END
                   CCY,
                SUM (BAL.LCY_AMOUNT) AS ALL_BALANCE_Place
         FROM flexcube.IATM_CATEGORY_GL_FUND_LINK GLS,
              flexcube.IATM_CATEGORY_GL_STATISTICS bal
         WHERE     GLS.CATEGORY_TYPE = 'ASSET'
               AND GLS.RECORD_STAT = 'O'
               AND GLS.AUTH_STAT = 'A'
               --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan
               AND GLS.FUND_GL_CATEGORY <> 'CRR'
               AND GLS.FUND_ID IN
                        (SELECT DISTINCT fund_id
                         FROM flexcube.IATM_POOL_BAL_ORDER
                         WHERE fund_id NOT IN
                                     (SELECT FUND_ID
                                      FROM flexcube.IATM_POOL_BAL_ERRORS
                                      WHERE ERROR_DESCRIPTION LIKE '%Error%'
                                            AND POOL_BAL_TYPE = 'D'
                                            AND run_date =
                                                  TO_DATE (:PM_DATE,
                                                           'YYYY-MM-DD')))
               AND GLS.START_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
               AND NVL (GLS.END_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
                     TO_DATE (:PM_DATE, 'YYYY-MM-DD')
               AND bal.CALC_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
               /*   (SELECT   MAX (M2.CALC_DATE)
                   FROM    flexcube.IATM_CATEGORY_GL_STATISTICS  M2
                  WHERE      -- bal.AC_BRANCH = M2.AC_BRANCH AND

                              bal.AC_CCY = M2.AC_CCY
                             AND bal.GL_CODE = M2.GL_CODE
                            and  bal.FUND_GL_CATEGORY=m2.FUND_GL_CATEGORY
                            and bal.CATEGORY_TYPE=m2.CATEGORY_TYPE
                            and m2.BAL_TYPE ='D'
                            AND M2.CALC_DATE <=  TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) */
               AND BAL.ac_branch IN (SELECT BRANCH_CODE
                                     FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                     WHERE IS_LIVE = 'Y')
               --IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
               AND BAL.AC_CCY = GLS.CURRENCY
               --and   GLS.CURRENCY in(:CCY)
               AND BAL.gl_code = GLS.GL_CODE
               AND gls.FUND_ID = bal.FUND_ID
               AND gls.fund_id = :PM_FUND_ID
               AND gls.currency = bal.AC_CCY
         GROUP BY GLS.GL_CODE,
                  GLS.GL_CODE_DESCRIPTION,
                  BAL.AC_CCY,
                  bal.FUND_ID) PLACE
     ON SUBSTR (MA.agl, 1, 8) = PLACE.GL AND MA.ACC_CCY = PLACE.CCY
-- where ACATEGORYS='IJARAH'
UNION ALL
SELECT 'CASH RESERVE' AS Categorys,
       'ALL' AS GL,
       base_ccy AS ASSET_CCY,
       NULL AS Description,
       SUM (NVL (S1.CASH_RESERVE, 0)) AS Balance
FROM flexcube.IATM_POOL_BAL_STATUS S1, flexcube.AMTM_FUND_MASTER pool
WHERE S1.RUN_DATE =
         (SELECT MAX (RUN_DATE)
          FROM flexcube.IATM_POOL_BAL_EXECUTION
          WHERE RUN_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                AND POOL_BAL_TYPE = 'D')
      AND pool.FUND_ID = s1.FUND_ID
      -- and pool.base_ccy in (:CCY)
      AND pool.fund_id = :PM_FUND_ID
      AND S1.POOL_BAL_TYPE = 'D'
      AND S1.FUND_ID = :PM_FUND_ID
GROUP BY 'CASH RESERVE', base_ccy
UNION ALL
-----NEW INVESTMENT QUERy

-----NEW INVESTMENT QUERy from zihaib
SELECT *
FROM (SELECT 'INVESTMENTS' AS Categorys,
             A.GL_CODE || ' USD' GL,
             --A.GL_CODE ||' '||asset_ccy GL ,
             asset_ccy CCY,
             c.asset_description Description,
             --  A.AVG_LCY_AMOUNT AS AVG_ADJ,B.PLCMNT,AMOUNT_IN_POOL,
             NVL (PLCMNT - AVG_LCY_AMOUNT + AMOUNT_IN_POOL, 0) Balance
      FROM       (SELECT FUND_ID,
                         RES.GL_CODE,
                         SUM (NET_ASSET_AMT) AS AVG_LCY_AMOUNT
                  FROM (SELECT S.GL_CODE GL_CODE,
                               SUM (NVL (M.ASSET_AMOUNT, S.PRINCIPAL_AMOUNT))
                                  AS NET_ASSET_AMT,
                               A.FUND_ID,
                               A.CURRENCY
                        FROM       flexcube.IATM_SUKKUKH_INPUT S
                                LEFT OUTER JOIN
                                   flexcube.IATM_ASSET_FUND_LINK_MASTER m
                                ON S.SUKKUKH_CODE = M.SUKKUKH_CODE
                                   AND S.SUKKUKH_CCY = M.ASSET_CCY
                             INNER JOIN
                                flexcube.IATM_CATEGORY_GL_FUND_LINK A
                             ON S.GL_CODE = A.GL_CODE
                                AND S.SUKKUKH_CCY = A.CURRENCY
                        -- CROSS JOIN (SELECT TRUNC ( to_date(TO_CHAR(LAST_DAY( TO_DATE (:PM_DATE, 'dd-MM-RRRR')), 'dd-MM-YYYY') , 'dd-MM-YYYY')  - LEVEL) + 1 AS VALUE_DATE FROM DUAL CONNECT BY LEVEL <=  to_date(TO_CHAR(LAST_DAY( TO_DATE (:PM_DATE, 'dd-MM-RRRR')), 'dd-MM-YYYY') , 'dd-MM-YYYY')  - TO_DATE (TO_CHAR (ADD_MONTHS ( to_date(TO_CHAR(LAST_DAY( TO_DATE (:PM_DATE, 'dd-MM-RRRR')), 'dd-MM-YYYY') , 'dd-MM-YYYY') , -1), 'YYYYMMDD'),'YYYYMMDD')) F
                        WHERE (M.EFF_DATE =
                                  (SELECT MAX (N.EFF_DATE)
                                   FROM flexcube.IATM_ASSET_FUND_LINK_MASTER N
                                   WHERE     M.ASSET_CCY = N.ASSET_CCY
                                         AND M.SUKKUKH_CODE = N.SUKKUKH_CODE
                                         AND M.ASSET_CODE = N.ASSET_CODE
                                         AND N.EFF_DATE <=
                                               TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                               OR M.EFF_DATE IS NULL)
                              AND NVL (S.ISSUE_DATE, '01-JAN-1900') <=
                                    TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                              AND NVL (S.MATURITY_DATE, '01-JAN-3000') >=
                                    TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                              AND S.RECORD_STAT = 'O'
                              AND S.AUTH_STAT = 'A'
                              AND A.RECORD_STAT = 'O'
                              AND A.AUTH_STAT = 'A'
                              AND A.CATEGORY_TYPE = 'ASSET'
                              AND A.FUND_GL_CATEGORY IN ('PLACEMENTS')
                        GROUP BY S.GL_CODE, A.FUND_ID, A.CURRENCY) RES
                  WHERE FUND_ID = :PM_FUND_ID
                  GROUP BY RES.GL_CODE, FUND_ID) a
              LEFT JOIN
                 (SELECT *
                  FROM (SELECT calc_date, gl_code, SUM (lcy_amount) PLCMNT
                        FROM flexcube.iatm_category_gl_statistics
                        WHERE     calc_date = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                              AND fund_id = :PM_FUND_ID
                              AND bal_type = 'D'
                        GROUP BY calc_date, gl_code)
                  WHERE plcmnt <> 0) B
              ON A.gl_code = B.gl_code
           LEFT JOIN
              (SELECT D.FUND_ID,
                      D.ASSET_CCY,
                      a.asset_description,
                      A.ASSET_CODE,
                      SUM(---Conversion of Liab,Asset and Sukkukh to LCY starts----
                          /*CYPKS_ADDON.fn_conv_amt1_to_amt2 (
                             Global.Current_Branch,
                             D.ASSET_CCY,
                             (SELECT BANK.BANK_LCY
                                FROM sttm_bank Bank),
                             'STANDARD',
                             'M',
                             D.ALLOCATED_AMOUNT,
                             'N')*/
                          (CASE D.ASSET_CCY
                              WHEN 'PKR'
                              THEN
                                 D.ALLOCATED_AMOUNT                 --B.AMOUNT
                              ELSE
                                 ( (D.ALLOCATED_AMOUNT)             --B.AMOUNT
                                  * (SELECT MAX (MID_RATE)
                                     FROM flexcube.CYTB_RATES_HISTORY H
                                     WHERE H.RATE_DATE =
                                              (SELECT MAX (E.RATE_DATE)
                                               FROM flexcube.CYTB_RATES_HISTORY E
                                               WHERE H.BRANCH_CODE =
                                                        E.BRANCH_CODE
                                                     AND H.CCY1 = E.CCY1
                                                     AND H.CCY2 = E.CCY2
                                                     AND E.RATE_TYPE = 'STANDARD'
                                                     AND E.RATE_DATE <=
                                                           TO_DATE (:PM_DATE,
                                                                    'YYYY-MM-DD'))
                                           AND H.CCY1 = D.ASSET_CCY
                                           AND H.CCY2 = 'PKR'
                                           AND H.RATE_TYPE = 'STANDARD'
                                           AND H.BRANCH_CODE = '951'--global.current_branch
                                    ))
                           END)---Conversion of Liab,Asset and Sukkukh to LCY ends----
                      )
                         AS AMOUNT_IN_POOL             --INTO l_sukkukh_amount
               FROM flexcube.IATM_ASSET_FUND_LINK_MASTER A,
                    flexcube.IATM_ASSET_FUND_LINK D,
                    flexcube.IATM_SUKKUKH_INPUT G
               WHERE A.EFF_DATE =
                        (SELECT MAX (B.EFF_DATE)
                         FROM flexcube.IATM_ASSET_FUND_LINK_MASTER B
                         WHERE     A.ASSET_CCY = B.ASSET_CCY
                               AND A.ASSET_CODE = B.ASSET_CODE
                               AND A.SUKKUKH_CODE = B.SUKKUKH_CODE
                               AND B.EFF_DATE <=
                                     TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                     AND D.EFF_DATE = A.EFF_DATE
                     AND D.ASSET_CODE = A.ASSET_CODE
                     AND D.ASSET_CCY = A.ASSET_CCY
                     AND D.SUKKUKH_CODE = A.SUKKUKH_CODE
                     AND A.SUKKUKH_CODE <> ' '
                     AND A.RECORD_STAT = 'O'
                     AND A.AUTH_STAT = 'A'
                     AND D.FUND_ID = :PM_FUND_ID
                     AND A.ASSET_CODE = G.GL_CODE
                     AND A.SUKKUKH_CODE = G.SUKKUKH_CODE
                     AND A.ASSET_CCY = G.SUKKUKH_CCY
                     AND NVL (G.MATURITY_DATE,
                              TO_DATE ('01/01/2999', 'DD/MM/YYYY')) >=
                           TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                     AND TO_DATE (:PM_DATE, 'YYYY-MM-DD') >=
                           NVL (G.ISSUE_DATE,
                                TO_DATE ('01/12/1800', 'DD/MM/YYYY'))
                     AND G.RECORD_STAT = 'O'
               GROUP BY A.ASSET_CODE,
                        a.asset_description,
                        D.ASSET_CCY,
                        D.FUND_ID) c
           ON A.gl_code = c.ASSET_CODE-- where a.gl_Code='40909503'

     )
---------------------------------------INVESTMENT END00000000--------------------------------------


---------------------------------INVESTMENT END00000000--------------------------------------
-----NEW INVESTMENT QUERy

                             /*  SELECT   'INVESTMENTS' AS Categorys , asset_code AS GL, asset_ccy AS CCY,asset_description AS Description,
                              NET_TAGED AS Balance

                            FROM (
 SELECT
  POOL_WISE.asset_code, POOL_WISE.asset_description, POOL_WISE.asset_ccy,

 CASE when:PM_FUND_ID = 'FCYPOOL' and UNTAGED.NET_PLACE > 0
 then ALL_BALANCE_PLACE
 WHEN :PM_FUND_ID <> 'FCYPOOL' and UNTAGED.NET_PLACE > 0 THEN  POOL_WISE.AMOUNT_IN_POOL+UNTAGED.NET_PLACE
ELSE  POOL_WISE.AMOUNT_IN_POOL  END   NET_TAGED

  FROM
(

SELECT GL_CODE,GL_DESCRIPTION,SUKKUKH_CCY,ALL_BALANCE_PLACE-ASSET_AMOUNT NET_PLACE,ALL_BALANCE_PLACE  FROM (
SELECT  GL_CODE,GL_DESCRIPTION,SUKKUKH_CCY,
 NVL(ASSET_AMOUNT_EVE,0) ASSET_AMOUNT,
NVL(ALL_BALANCE_PLACE,0) ALL_BALANCE_PLACE

FROM (

SELECT GL_CODE,GL_DESCRIPTION,SUKKUKH_CCY,SUM(ASSET_AMOUNT_EVE)ASSET_AMOUNT_EVE FROM (
SELECT GL_CODE,GL_DESCRIPTION,SUKKUKH_CCY,SUKKUKH_CODE,
    PRINCIPAL_AMOUNT  PRINCIPAL_AMOUNT,   ASSET_AMOUNT  ASSET_AMOUNT,
     CASE
 WHEN NVL (ASSET_AMOUNT, 0) = 0 THEN PRINCIPAL_AMOUNT WHEN PRINCIPAL_AMOUNT <>ASSET_AMOUNT THEN ASSET_AMOUNT ELSE PRINCIPAL_AMOUNT  END  ASSET_AMOUNT_EVE
     FROM (
 SELECT SUKKUKH_INPUT.GL_CODE,SUKKUKH_INPUT.GL_DESCRIPTION,SUKKUKH_INPUT.SUKKUKH_CODE,SUKKUKH_INPUT.SUKKUKH_CCY,
   NVL(SUKKUKH_INPUT.PRINCIPAL_AMOUNT,0) PRINCIPAL_AMOUNT,
   NVL( FUND_LINK_MASTER.AMOUNT_IN_POOL,0) ASSET_AMOUNT FROM (
 SELECT  GL_CODE,GL_DESCRIPTION,SUKKUKH_CODE ,SUKKUKH_CCY,SUM(PRINCIPAL_AMOUNT) PRINCIPAL_AMOUNT
                 FROM  FLEXCUBE.IATM_SUKKUKH_INPUT
                        WHERE RECORD_STAT='O'
                        AND AUTH_STAT='A'
                       -- AND SUKKUKH_CCY IN (:CCY)
                        AND  NVL (ISSUE_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) <=  TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                        AND NVL (MATURITY_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) >= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                 GROUP BY    GL_CODE,GL_DESCRIPTION ,SUKKUKH_CCY ,SUKKUKH_CODE
 )SUKKUKH_INPUT
    LEFT OUTER JOIN
  -------------------------------------------------GET ALL THE SUKK FROM MASTER TABLE
   (
        SELECT     a.asset_code , a.asset_description,A.SUKKUKH_CODE, a.asset_ccy,   sum( ASSET_AMOUNT) AMOUNT_IN_POOL

                                      FROM   flexcube.IATM_ASSET_FUND_LINK_MASTER A
                                     WHERE   A.EFF_DATE =
                                                (SELECT   MAX (B.EFF_DATE)
                                                    FROM    flexcube.IATM_ASSET_FUND_LINK_MASTER B
                                                  WHERE       A.ASSET_CCY = B.ASSET_CCY
                                                          AND A.ASSET_CODE = B.ASSET_CODE
                                                          AND A.SUKKUKH_CODE = B.SUKKUKH_CODE
                                                          AND B.EFF_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')  )
                                           -- AND A.ASSET_CCY IN (:CCY)
                                             AND A.SUKKUKH_CODE <> ' '
                                             AND A.RECORD_STAT = 'O'
                                             AND A.AUTH_STAT = 'A'
     GROUP BY A.ASSET_CODE, A.ASSET_DESCRIPTION, A.ASSET_CCY ,A.SUKKUKH_CODE
                                              )FUND_LINK_MASTER

      ON
           FUND_LINK_MASTER.asset_code=SUKKUKH_INPUT.GL_CODE
      AND  FUND_LINK_MASTER.SUKKUKH_CODE=SUKKUKH_INPUT.SUKKUKH_CODE
    AND FUND_LINK_MASTER.asset_ccy=SUKKUKH_INPUT.SUKKUKH_CCY
      ))GROUP BY GL_CODE,GL_DESCRIPTION,SUKKUKH_CCY  ) SUKK

       LEFT JOIN
       ---------------------------------------------------------PLACEMENT QUERY
       (
        SELECT
           GLS.GL_CODE AS GL,
           GLS.GL_CODE_DESCRIPTION AS GL_DESC,
          -- BAL.AC_CCY CCY,
           SUM (BAL.LCY_AMOUNT) AS ALL_BALANCE_PLACE

    FROM   flexcube.IATM_CATEGORY_GL_FUND_LINK  GLS, flexcube.IATM_CATEGORY_GL_STATISTICS bal
   WHERE       GLS.CATEGORY_TYPE = 'ASSET'
   and BAL_TYPE='D'
           AND GLS.RECORD_STAT = 'O' AND GLS.AUTH_STAT = 'A' --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan
           AND GLS.FUND_GL_CATEGORY <> 'CRR'
           AND GLS.FUND_ID IN (select distinct fund_id from flexcube.IATM_POOL_BAL_ORDER  where
            fund_id not in (select FUND_ID from flexcube.IATM_POOL_BAL_ERRORS  where  POOL_BAL_TYPE ='D' and run_date = TO_DATE (:PM_DATE, 'YYYY-MM-DD')))
           AND GLS.START_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
           AND NVL (GLS.END_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
                TO_DATE (:PM_DATE, 'YYYY-MM-DD')
           AND BAL.ac_branch IN (SELECT BRANCH_CODE FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM  WHERE IS_LIVE = 'Y')--IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
           AND BAL.AC_CCY = GLS.CURRENCY
          -- and   GLS.CURRENCY in(:CCY)
           AND CALC_DATE= TO_DATE (:PM_DATE, 'YYYY-MM-DD') /*(SELECT   MAX (B.CALC_DATE)
                                                    FROM   flexcube.IATM_CATEGORY_GL_STATISTICS B
                                                  WHERE       BAL.AC_CCY = B.AC_CCY
                                                          AND BAL.GL_CODE = B.GL_CODE
                                                          AND B.CALC_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD'))*/
         /*  AND BAL.gl_code = GLS.GL_CODE and
           gls.FUND_ID=bal.FUND_ID and
           bal.FUND_ID=:PM_FUND_ID
          AND gls.currency=bal.AC_CCY
group by   GLS.GL_CODE, GLS.GL_CODE_DESCRIPTION--, BAL.AC_CCY
         )PLACE
       ON SUKK.GL_CODE=PLACE.GL
     --  AND SUKK.SUKKUKH_CCY=PLACE.CCY

        ))UNTAGED
  ---------------------------------------------------------------------------------------------------------
  ------------------------------TAGED ASSET POOL WISE------------------------------------------------------
RIGHT JOIN
 (

  -------------FOR POOL OTHER THN FCY
  SELECT   a.asset_code, a.asset_description, a.asset_ccy,   sum( D.ALLOCATED_AMOUNT) AMOUNT_IN_POOL
         FROM   flexcube.IATM_ASSET_FUND_LINK_MASTER A, flexcube.IATM_ASSET_FUND_LINK D
                                     WHERE   A.EFF_DATE =
                                                (SELECT   MAX (B.EFF_DATE)
                                                    FROM    flexcube.IATM_ASSET_FUND_LINK B
                                                  WHERE       A.ASSET_CCY = B.ASSET_CCY
                                                          AND A.ASSET_CODE = B.ASSET_CODE
                                                          AND A.SUKKUKH_CODE = B.SUKKUKH_CODE
                                                          AND B.EFF_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')

                                                          )
                                             AND D.EFF_DATE = A.EFF_DATE
                                             AND D.ASSET_CODE = A.ASSET_CODE
                                             AND D.ASSET_CCY = A.ASSET_CCY
                                             AND D.SUKKUKH_CODE = A.SUKKUKH_CODE
                                             AND A.SUKKUKH_CODE <> ' '

                                             AND A.RECORD_STAT = 'O'
                                             AND A.AUTH_STAT = 'A'
                                             AND D.FUND_ID IN (select distinct fund_id from flexcube.IATM_POOL_BAL_ORDER
                                             where fund_id not in (select FUND_ID from flexcube.IATM_POOL_BAL_ERRORS where POOL_BAL_TYPE ='D'
                                             and run_date = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                             and ERROR_DESCRIPTION like '%Error%' )

                                            AND D.FUND_ID= :PM_FUND_ID
                                             )
    GROUP BY A.ASSET_CODE, A.ASSET_DESCRIPTION, A.ASSET_CCY



   )POOL_WISE

   ON POOL_WISE.asset_code=UNTAGED.GL_CODE
   AND  POOL_WISE.asset_ccy=UNTAGED.SUKKUKH_CCY
 )
 */
---------------------------------------INVESTMENT END00000000--------------------------------------
----Placements
UNION ALL
SELECT GLS.FUND_GL_CATEGORY AS ACategorys,
       GLS.GL_CODE || ' ' || BAL.AC_CCY AS AGL,
       BAL.AC_CCY ACC_CCY,
       GLS.GL_CODE_DESCRIPTION AS ADescription,
       SUM (BAL.LCY_AMOUNT) AS ABalance
FROM flexcube.IATM_CATEGORY_GL_FUND_LINK GLS,
     flexcube.IATM_CATEGORY_GL_STATISTICS bal
WHERE                                        --GLS.CATEGORY_TYPE = 'ASSET' and
     GLS  .FUND_GL_CATEGORY <> 'CRR'
      AND BAL.gl_code = GLS.GL_CODE
      AND BAL.AC_CCY = GLS.CURRENCY
      -- and   GLS.CURRENCY in(:CCY)
      AND gls.FUND_ID = bal.FUND_ID
      AND GLS.FUND_ID = :PM_FUND_ID
      AND GLS.START_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      AND NVL (GLS.END_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
            TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan

      AND bal.CALC_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      /*(SELECT   MAX (M2.CALC_DATE)
       FROM    flexcube.IATM_CATEGORY_GL_STATISTICS  M2
      WHERE       --bal.AC_BRANCH = M2.AC_BRANCH
                 --AND
                 bal.AC_CCY = M2.AC_CCY
                 AND bal.GL_CODE = M2.GL_CODE
                and  bal.FUND_GL_CATEGORY=m2.FUND_GL_CATEGORY
                and bal.CATEGORY_TYPE=m2.CATEGORY_TYPE
                and m2.BAL_TYPE ='D'
                AND M2.CALC_DATE <=  TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) */
      AND BAL.ac_branch IN (SELECT BRANCH_CODE
                            FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                            WHERE IS_LIVE = 'Y')
      --IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
      AND BAL.BAL_TYPE = 'D'
      AND GLS.CATEGORY_TYPE = 'ASSET'
      AND GLS.RECORD_STAT = 'O'
      AND GLS.AUTH_STAT = 'A'
      AND gls.gl_Code NOT IN
               (                                                    --40552001
                SELECT AGL
                FROM (SELECT ACategorys,
                             AGL,
                             ACC_CCY,
                             ADescription,
                             ABalance + NVL (ALL_BALANCE_Place, 0) ABalance
                      FROM    (/*SELECT   GLS.FUND_GL_CATEGORY AS ACategorys,
                                          GLS.GL_CODE AS AGL,
                                          BAL.ACC_CCY,
                                          GLS.GL_CODE_DESCRIPTION AS ADescription,
                                          -SUM (BAL.LCY_CLOSING_BAL) AS ABalance
                                   FROM   flexcube.IATM_CATEGORY_GL_FUND_LINK  GLS, flexcube.actb_accbal_history BAL --TEMP_actb_accbal_history BAL
                                  WHERE       GLS.CATEGORY_TYPE = 'ASSET'
                                          AND GLS.RECORD_STAT = 'O' --System even picking closed GL link maintenances. check for only open records Change Added by: Zeeshan
                                          AND GLS.FUND_GL_CATEGORY <> 'CASH'
                                          AND GLS.FUND_ID = :PM_FUND_ID
                                          AND GLS.START_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                          AND NVL (GLS.END_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
                                               TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                          AND BAL.ACCOUNT = GLS.GL_CODE
                                          AND BAL.ACC_CCY=GLS.CURRENCY
                                          AND BAL.BKG_DATE =   (SELECT   MAX (BAL2.BKG_DATE)
                                                   FROM    flexcube.actb_accbal_history BAL2 --TEMP_actb_accbal_history BAL2
                                                  WHERE       BAL2.ACCOUNT = BAL.ACCOUNT
                                                          AND BAL2.ACC_CCY = BAL.ACC_CCY
                                                          AND BAL2.BRANCH_CODE = BAL.BRANCH_CODE
                                                           AND
                                                           BAL2.BKG_DATE <=
                                                                TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                               GROUP BY   GLS.FUND_GL_CATEGORY,BAL.ACC_CCY, GLS.GL_CODE, GLS.GL_CODE_DESCRIPTION
                               */
                               --UNION ALL
                               SELECT a1.PRODUCT_CATEGORY AS ACategorys,
                                      p1.ACCOUNT_HEAD AGL,
                                      d1.ASSET_CCY ACC_CCY,
                                      G1.GL_DESC AS ADescription,
                                      SUM (D1.ALLOCATED_AMOUNT) AS ABalance
                               FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M1,
                                    flexcube.IATM_ASSET_FUND_LINK D1,
                                    flexcube.CLTB_ACCOUNT_APPS_MASTER A1,
                                    flexcube.cltm_product_rth P1,
                                    flexcube.GLTM_GLMASTER G1
                               WHERE M1.EFF_DATE =
                                        (SELECT MAX (M2.eff_date)
                                         FROM flexcube.IATM_ASSET_FUND_LINK_MASTER M2
                                         WHERE M2.EFF_DATE <=
                                                  TO_DATE (:PM_DATE,
                                                           'YYYY-MM-DD')
                                               AND M2.ASSET_CODE = M1.ASSET_CODE
                                               AND M2.SUKKUKH_CODE = ' '
                                               AND M2.ASSET_CCY = M1.ASSET_CCY
                                               AND M2.AUTH_STAT = 'A'
                                               AND M2.RECORD_STAT = 'O')
                                     AND M1.EFF_DATE = D1.EFF_DATE
                                     -- AND a1.USER_DEFINED_STATUS = 'NORM'
                                     AND M1.ASSET_CODE = D1.ASSET_CODE
                                     AND D1.FUND_ID = :PM_FUND_ID
                                     --  and M1.asset_code in(:CCY)
                                     AND M1.SUKKUKH_CODE = D1.SUKKUKH_CODE
                                     AND M1.ASSET_CCY = D1.ASSET_CCY
                                     AND M1.AUTH_STAT = 'A'
                                     AND M1.RECORD_STAT = 'O'
                                     AND M1.ASSET_CODE = A1.ACCOUNT_NUMBER
                                     AND A1.PRODUCT_CODE = P1.PRODUCT_CODE
                                     AND P1.ACCOUNTING_ROLE = 'LOAN_ACCOUNT'
                                     AND G1.GL_CODE = P1.ACCOUNT_HEAD
                                     AND A1.Branch_code IN
                                              (SELECT BRANCH_CODE
                                               FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                               WHERE IS_LIVE = 'Y')
                               --in (select to_char(OFF_CODE) from ask_uni.branches_area where flexcube='I')
                               GROUP BY A1.PRODUCT_CATEGORY,
                                        P1.ACCOUNT_HEAD,
                                        G1.GL_DESC,
                                        D1.ASSET_CCY
                               -----
                               /*
                               SELECT   CL.PRODUCT_CATEGORY AS Categorys,
                                          RTH.ACCOUNT_HEAD||' '||M1.ASSET_CCY AS GL,
                                          M1.ASSET_CCY,
                                          GL.GL_DESC AS Description,
                                          SUM (D.ALLOCATED_AMOUNT) AS Balance
                                   FROM   flexcube.IATM_ASSET_FUND_LINK_MASTER  M1,
                                          flexcube.IATM_ASSET_FUND_LINK  D,
                                          flexcube.CLTB_ACCOUNT_APPS_MASTER CL,
                                          flexcube.CLTM_PRODUCT_RTH RTH,
                                          flexcube.GLTM_GLMASTER GL
                                  WHERE   M1.EFF_DATE =
                                           (SELECT   MAX (M2.eff_date)
                                                     FROM      flexcube.IATM_ASSET_FUND_LINK_MASTER  M2
                                                    WHERE       M2.EFF_DATE <=   TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                                            AND M2.ASSET_CODE = M1.ASSET_CODE
                                                            AND M2.SUKKUKH_CODE = ' '
                                                            AND M2.ASSET_CCY = M1.ASSET_CCY
                                                            AND M2.AUTH_STAT = 'A'
                                                            AND M2.RECORD_STAT = 'O')

                                          --AND D.FUND_ID = :PM_FUND_ID
                                         AND CL.USER_DEFINED_STATUS = 'NORM'
                                          AND M1.EFF_DATE = D.EFF_DATE
                                          AND M1.ASSET_CODE = D.ASSET_CODE
                                          AND M1.ASSET_CCY = D.ASSET_CCY
                                          AND M1.SUKKUKH_CODE = D.SUKKUKH_CODE
                                          AND M1.ASSET_CODE = CL.ACCOUNT_NUMBER
                                          AND CL.PRODUCT_CODE = RTH.PRODUCT_CODE
                                          AND RTH.ACCOUNTING_ROLE = 'LOAN_ACCOUNT'
                                          AND M1.RECORD_STAT = 'O'
                                          AND M1.AUTH_STAT = 'A'
                                          AND GL.GL_CODE = RTH.ACCOUNT_HEAD
                                           and cl.Branch_code IN (SELECT BRANCH_CODE FROM    flexcube.STTM_BRANCH_ISLAMIC_CUSTOM  WHERE IS_LIVE = 'Y')
                               GROUP BY   CL.PRODUCT_CATEGORY, RTH.ACCOUNT_HEAD, GL.GL_DESC,M1.ASSET_CCY
                                */
                               /*
                               union all
                                SELECT   'INVESTMENTS' AS Categorys,
                                         M1.ASSET_CODE ||' '||M1.ASSET_CCY AS GL,
                                         M1.ASSET_CCY,
                                         M1.ASSET_DESCRIPTION AS Description,
                                         SUM (D.ALLOCATED_AMOUNT) AS Balance
                                  FROM   flexcube.IATM_ASSET_FUND_LINK_MASTER  M1,
                                         flexcube.IATM_ASSET_FUND_LINK  D,
                                         flexcube.GLTM_GLMASTER GLM
                                 WHERE   M1.EFF_DATE =
                                            (SELECT   MAX (M2.EFF_DATE)
                                               FROM   flexcube.IATM_ASSET_FUND_LINK_MASTER  M2
                                              WHERE       M2.ASSET_CODE = M1.ASSET_CODE
                                                      AND M2.ASSET_CCY = M1.ASSET_CCY
                                                      AND M2.SUKKUKH_CODE = M1.SUKKUKH_CODE
                                                      AND M2.EFF_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD'))
                                         aND D.FUND_ID = :PM_FUND_ID
                                         AND M1.EFF_DATE = D.EFF_DATE
                                         AND M1.ASSET_CODE = D.ASSET_CODE
                                         AND M1.ASSET_CCY = D.ASSET_CCY
                                         AND M1.SUKKUKH_CODE = D.SUKKUKH_CODE
                                         AND D.ASSET_CODE = GLM.GL_CODE
                              GROUP BY   M1.ASSET_CODE, M1.ASSET_DESCRIPTION,M1.ASSET_CCY
                               */


                               UNION ALL
                               SELECT 'CASH RESERVE' AS Categorys,
                                      'ALL' AS GL,
                                      base_ccy AS ASSET_CCY,
                                      NULL AS Description,
                                      SUM (NVL (S1.CASH_RESERVE, 0)) AS Balance
                               FROM flexcube.IATM_POOL_BAL_STATUS S1,
                                    flexcube.AMTM_FUND_MASTER pool
                               WHERE S1.RUN_DATE =
                                        (SELECT MAX (RUN_DATE)
                                         FROM flexcube.IATM_POOL_BAL_EXECUTION
                                         WHERE RUN_DATE =
                                                  TO_DATE (:PM_DATE,
                                                           'YYYY-MM-DD')
                                               AND POOL_BAL_TYPE = 'D')
                                     AND pool.FUND_ID = s1.FUND_ID
                                     -- and pool.base_ccy in (:CCY)
                                     AND pool.fund_id = :PM_FUND_ID
                                     AND S1.POOL_BAL_TYPE = 'D'
                                     AND S1.FUND_ID = :PM_FUND_ID
                               GROUP BY 'CASH RESERVE', base_ccy) MA
                           LEFT JOIN
                              (SELECT GLS.GL_CODE AS GL,
                                      GLS.GL_CODE_DESCRIPTION AS GL_DESC,
                                      BAL.AC_CCY CCY,
                                      SUM (BAL.LCY_AMOUNT) AS ALL_BALANCE_Place
                               FROM flexcube.IATM_CATEGORY_GL_FUND_LINK GLS,
                                    flexcube.IATM_CATEGORY_GL_STATISTICS bal
                               WHERE     GLS.CATEGORY_TYPE = 'ASSET'
                                     AND GLS.RECORD_STAT = 'O'
                                     AND GLS.AUTH_STAT = 'A'
                                     --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan
                                     AND GLS.FUND_GL_CATEGORY <> 'CRR'
                                     AND GLS.FUND_ID IN
                                              (SELECT DISTINCT fund_id
                                               FROM flexcube.IATM_POOL_BAL_ORDER
                                               WHERE fund_id NOT IN
                                                           (SELECT FUND_ID
                                                            FROM flexcube.IATM_POOL_BAL_ERRORS
                                                            WHERE ERROR_DESCRIPTION LIKE
                                                                     '%Error%'
                                                                  AND POOL_BAL_TYPE =
                                                                        'D'
                                                                  AND run_date =
                                                                        TO_DATE (
                                                                           :PM_DATE,
                                                                           'YYYY-MM-DD'
                                                                        )))
                                     AND GLS.START_DATE <=
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                     AND NVL (GLS.END_DATE,
                                              TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                     AND bal.CALC_DATE =
                                           TO_DATE (:PM_DATE, 'YYYY-MM-DD')
                                     /* (SELECT   MAX (M2.CALC_DATE)
                                       FROM    flexcube.IATM_CATEGORY_GL_STATISTICS  M2
                                      WHERE      -- bal.AC_BRANCH = M2.AC_BRANCH AND

                                                  bal.AC_CCY = M2.AC_CCY
                                                 AND bal.GL_CODE = M2.GL_CODE
                                                and  bal.FUND_GL_CATEGORY=m2.FUND_GL_CATEGORY
                                                and bal.CATEGORY_TYPE=m2.CATEGORY_TYPE
                                                and m2.BAL_TYPE ='D'
                                                AND M2.CALC_DATE <=  TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) */
                                     AND BAL.ac_branch IN
                                              (SELECT BRANCH_CODE
                                               FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                               WHERE IS_LIVE = 'Y')
                                     --IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
                                     AND BAL.AC_CCY = GLS.CURRENCY
                                     --and   GLS.CURRENCY in(:CCY)
                                     AND BAL.gl_code = GLS.GL_CODE
                                     AND gls.FUND_ID = bal.FUND_ID
                                     AND gls.fund_id = :PM_FUND_ID
                                     AND gls.currency = bal.AC_CCY
                               GROUP BY GLS.GL_CODE,
                                        GLS.GL_CODE_DESCRIPTION,
                                        BAL.AC_CCY) PLACE
                           ON SUBSTR (MA.agl, 1, 8) = PLACE.GL
                              AND MA.ACC_CCY = PLACE.CCY
                      -- where ACATEGORYS='IJARAH'

                      UNION ALL
                      -----NEW INVESTMENT QUERy

                      SELECT 'INVESTMENTS' AS Categorys,
                             asset_code AS GL,
                             asset_ccy AS CCY,
                             asset_description AS Description,
                             NET_TAGED AS Balance
                      FROM (SELECT POOL_WISE.asset_code,
                                   POOL_WISE.asset_description,
                                   POOL_WISE.asset_ccy,
                                   CASE
                                      WHEN :PM_FUND_ID = 'FCYPOOL'
                                      THEN
                                         ALL_BALANCE_PLACE - AMOUNT_IN_POOL
                                      WHEN :PM_FUND_ID <> 'FCYPOOL'
                                           AND UNTAGED.NET_PLACE > 0
                                      THEN
                                         POOL_WISE.AMOUNT_IN_POOL
                                         + UNTAGED.NET_PLACE
                                      ELSE
                                         POOL_WISE.AMOUNT_IN_POOL
                                   END
                                      NET_TAGED
                            FROM    (SELECT GL_CODE,
                                            GL_DESCRIPTION,
                                            SUKKUKH_CCY,
                                            ALL_BALANCE_PLACE - ASSET_AMOUNT
                                               NET_PLACE,
                                            ALL_BALANCE_PLACE
                                     FROM (SELECT GL_CODE,
                                                  GL_DESCRIPTION,
                                                  SUKKUKH_CCY,
                                                  NVL (ASSET_AMOUNT_EVE, 0)
                                                     ASSET_AMOUNT,
                                                  NVL (ALL_BALANCE_PLACE, 0)
                                                     ALL_BALANCE_PLACE
                                           FROM    (SELECT GL_CODE,
                                                           GL_DESCRIPTION,
                                                           SUKKUKH_CCY,
                                                           SUM (ASSET_AMOUNT_EVE)
                                                              ASSET_AMOUNT_EVE
                                                    FROM (SELECT GL_CODE,
                                                                 GL_DESCRIPTION,
                                                                 SUKKUKH_CCY,
                                                                 SUKKUKH_CODE,
                                                                 PRINCIPAL_AMOUNT
                                                                    PRINCIPAL_AMOUNT,
                                                                 ASSET_AMOUNT
                                                                    ASSET_AMOUNT,
                                                                 CASE
                                                                    WHEN NVL (
                                                                            ASSET_AMOUNT,
                                                                            0
                                                                         ) = 0
                                                                    THEN
                                                                       PRINCIPAL_AMOUNT
                                                                    WHEN PRINCIPAL_AMOUNT <>
                                                                            ASSET_AMOUNT
                                                                    THEN
                                                                       ASSET_AMOUNT
                                                                    ELSE
                                                                       PRINCIPAL_AMOUNT
                                                                 END
                                                                    ASSET_AMOUNT_EVE
                                                          FROM (SELECT SUKKUKH_INPUT.GL_CODE,
                                                                       SUKKUKH_INPUT.GL_DESCRIPTION,
                                                                       SUKKUKH_INPUT.SUKKUKH_CODE,
                                                                       SUKKUKH_INPUT.SUKKUKH_CCY,
                                                                       NVL (
                                                                          SUKKUKH_INPUT.PRINCIPAL_AMOUNT,
                                                                          0
                                                                       )
                                                                          PRINCIPAL_AMOUNT,
                                                                       NVL (
                                                                          FUND_LINK_MASTER.AMOUNT_IN_POOL,
                                                                          0
                                                                       )
                                                                          ASSET_AMOUNT
                                                                FROM    (SELECT GL_CODE,
                                                                                GL_DESCRIPTION,
                                                                                SUKKUKH_CODE,
                                                                                SUKKUKH_CCY,
                                                                                SUM(PRINCIPAL_AMOUNT)
                                                                                   PRINCIPAL_AMOUNT
                                                                         FROM FLEXCUBE.IATM_SUKKUKH_INPUT
                                                                         WHERE RECORD_STAT =
                                                                                  'O'
                                                                               AND AUTH_STAT =
                                                                                     'A'
                                                                               -- AND SUKKUKH_CCY IN (:CCY)
                                                                               AND NVL (
                                                                                     ISSUE_DATE,
                                                                                     TO_DATE (
                                                                                        :PM_DATE,
                                                                                        'YYYY-MM-DD'
                                                                                     )
                                                                                  ) <=
                                                                                     TO_DATE (
                                                                                        :PM_DATE,
                                                                                        'YYYY-MM-DD'
                                                                                     )
                                                                               AND NVL (
                                                                                     MATURITY_DATE,
                                                                                     TO_DATE (
                                                                                        :PM_DATE,
                                                                                        'YYYY-MM-DD'
                                                                                     )
                                                                                  ) >=
                                                                                     TO_DATE (
                                                                                        :PM_DATE,
                                                                                        'YYYY-MM-DD'
                                                                                     )
                                                                         GROUP BY GL_CODE,
                                                                                  GL_DESCRIPTION,
                                                                                  SUKKUKH_CCY,
                                                                                  SUKKUKH_CODE)
                                                                        SUKKUKH_INPUT
                                                                     LEFT OUTER JOIN
                                                                        -------------------------------------------------GET ALL THE SUKK FROM MASTER TABLE
                                                                        (SELECT a.asset_code,
                                                                                a.asset_description,
                                                                                A.SUKKUKH_CODE,
                                                                                a.asset_ccy,
                                                                                SUM(ASSET_AMOUNT)
                                                                                   AMOUNT_IN_POOL
                                                                         FROM flexcube.IATM_ASSET_FUND_LINK_MASTER A
                                                                         WHERE A.EFF_DATE =
                                                                                  (SELECT MAX(B.EFF_DATE)
                                                                                   FROM flexcube.IATM_ASSET_FUND_LINK_MASTER B
                                                                                   WHERE A.ASSET_CCY =
                                                                                            B.ASSET_CCY
                                                                                         AND A.ASSET_CODE =
                                                                                               B.ASSET_CODE
                                                                                         AND A.SUKKUKH_CODE =
                                                                                               B.SUKKUKH_CODE
                                                                                         AND B.EFF_DATE <=
                                                                                               TO_DATE (
                                                                                                  :PM_DATE,
                                                                                                  'YYYY-MM-DD'
                                                                                               ))
                                                                               -- AND A.ASSET_CCY IN (:CCY)
                                                                               AND A.SUKKUKH_CODE <>
                                                                                     ' '
                                                                               AND A.RECORD_STAT =
                                                                                     'O'
                                                                               AND A.AUTH_STAT =
                                                                                     'A'
                                                                         GROUP BY A.ASSET_CODE,
                                                                                  A.ASSET_DESCRIPTION,
                                                                                  A.ASSET_CCY,
                                                                                  A.SUKKUKH_CODE)
                                                                        FUND_LINK_MASTER
                                                                     ON FUND_LINK_MASTER.asset_code =
                                                                           SUKKUKH_INPUT.GL_CODE
                                                                        AND FUND_LINK_MASTER.SUKKUKH_CODE =
                                                                              SUKKUKH_INPUT.SUKKUKH_CODE
                                                                        AND FUND_LINK_MASTER.asset_ccy =
                                                                              SUKKUKH_INPUT.SUKKUKH_CCY))
                                                    GROUP BY GL_CODE,
                                                             GL_DESCRIPTION,
                                                             SUKKUKH_CCY) SUKK
                                                LEFT JOIN
                                                   ---------------------------------------------------------PLACEMENT QUERY
                                                   (SELECT GLS.GL_CODE AS GL,
                                                           GLS.GL_CODE_DESCRIPTION
                                                              AS GL_DESC,
                                                           --BAL.AC_CCY CCY, commented on 04-0919
                                                           SUM (BAL.LCY_AMOUNT)
                                                              AS ALL_BALANCE_PLACE
                                                    FROM flexcube.IATM_CATEGORY_GL_FUND_LINK GLS,
                                                         flexcube.IATM_CATEGORY_GL_STATISTICS bal
                                                    WHERE GLS.CATEGORY_TYPE =
                                                             'ASSET'
                                                          AND BAL_TYPE = 'D'
                                                          AND GLS.RECORD_STAT =
                                                                'O'
                                                          AND GLS.AUTH_STAT = 'A'
                                                          --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan
                                                          AND GLS.FUND_GL_CATEGORY <>
                                                                'CRR'
                                                          AND GLS.FUND_ID IN
                                                                   (SELECT DISTINCT
                                                                           fund_id
                                                                    FROM flexcube.IATM_POOL_BAL_ORDER
                                                                    WHERE fund_id NOT IN
                                                                                (SELECT FUND_ID
                                                                                 FROM flexcube.IATM_POOL_BAL_ERRORS
                                                                                 WHERE POOL_BAL_TYPE =
                                                                                          'D'
                                                                                       AND run_date =
                                                                                             TO_DATE (
                                                                                                :PM_DATE,
                                                                                                'YYYY-MM-DD'
                                                                                             )))
                                                          AND GLS.START_DATE <=
                                                                TO_DATE (
                                                                   :PM_DATE,
                                                                   'YYYY-MM-DD'
                                                                )
                                                          AND NVL (
                                                                GLS.END_DATE,
                                                                TO_DATE (
                                                                   :PM_DATE,
                                                                   'YYYY-MM-DD'
                                                                )
                                                             ) >=
                                                                TO_DATE (
                                                                   :PM_DATE,
                                                                   'YYYY-MM-DD'
                                                                )
                                                          AND BAL.ac_branch IN
                                                                   (SELECT BRANCH_CODE
                                                                    FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                                                                    WHERE IS_LIVE =
                                                                             'Y')
                                                          --IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
                                                          AND BAL.AC_CCY =
                                                                GLS.CURRENCY
                                                          -- and   GLS.CURRENCY in(:CCY)
                                                          AND CALC_DATE =
                                                                TO_DATE (
                                                                   :PM_DATE,
                                                                   'YYYY-MM-DD'
                                                                )
                                                          /*(SELECT   MAX (B.CALC_DATE)
                                                                                                        FROM   flexcube.IATM_CATEGORY_GL_STATISTICS  B
                                                                                                      WHERE       BAL.AC_CCY = B.AC_CCY
                                                                                                              AND BAL.GL_CODE = B.GL_CODE
                                                                                                              AND B.CALC_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD'))*/
                                                          AND BAL.gl_code =
                                                                GLS.GL_CODE
                                                          AND gls.FUND_ID =
                                                                bal.FUND_ID
                                                          AND bal.FUND_ID =
                                                                :PM_FUND_ID
                                                          AND gls.currency =
                                                                bal.AC_CCY
                                                    GROUP BY GLS.GL_CODE,
                                                             GLS.GL_CODE_DESCRIPTION--, BAL.AC_CCY -- commented on 04-0919
                                                   ) PLACE
                                                ON SUKK.GL_CODE = PLACE.GL-- AND SUKK.SUKKUKH_CCY=PLACE.CCY commented on 04-0919

                                          )) UNTAGED
                                 ---------------------------------------------------------------------------------------------------------
                                 ------------------------------TAGED ASSET POOL WISE------------------------------------------------------
                                 RIGHT JOIN
                                    (-------------FOR POOL OTHER THN FCY
                                     SELECT a.asset_code,
                                            a.asset_description,
                                            a.asset_ccy,
                                            SUM (D.ALLOCATED_AMOUNT)
                                               AMOUNT_IN_POOL
                                     FROM flexcube.IATM_ASSET_FUND_LINK_MASTER A,
                                          flexcube.IATM_ASSET_FUND_LINK D
                                     WHERE A.EFF_DATE =
                                              (SELECT MAX (B.EFF_DATE)
                                               FROM flexcube.IATM_ASSET_FUND_LINK B
                                               WHERE A.ASSET_CCY = B.ASSET_CCY
                                                     AND A.ASSET_CODE =
                                                           B.ASSET_CODE
                                                     AND A.SUKKUKH_CODE =
                                                           B.SUKKUKH_CODE
                                                     AND B.EFF_DATE <=
                                                           TO_DATE (:PM_DATE,
                                                                    'YYYY-MM-DD'))
                                           AND D.EFF_DATE = A.EFF_DATE
                                           AND D.ASSET_CODE = A.ASSET_CODE
                                           AND D.ASSET_CCY = A.ASSET_CCY
                                           AND D.SUKKUKH_CODE = A.SUKKUKH_CODE
                                           AND A.SUKKUKH_CODE <> ' '
                                           AND A.RECORD_STAT = 'O'
                                           AND A.AUTH_STAT = 'A'
                                           AND D.FUND_ID IN
                                                    (SELECT DISTINCT fund_id
                                                     FROM flexcube.IATM_POOL_BAL_ORDER
                                                     WHERE fund_id NOT IN
                                                                 (SELECT FUND_ID
                                                                  FROM flexcube.IATM_POOL_BAL_ERRORS
                                                                  WHERE POOL_BAL_TYPE =
                                                                           'D'
                                                                        AND run_date =
                                                                              TO_DATE (
                                                                                 :PM_DATE,
                                                                                 'YYYY-MM-DD'
                                                                              )
                                                                        AND ERROR_DESCRIPTION LIKE
                                                                              '%Error%')
                                                           AND D.FUND_ID =
                                                                 :PM_FUND_ID)
                                     GROUP BY A.ASSET_CODE,
                                              A.ASSET_DESCRIPTION,
                                              A.ASSET_CCY) POOL_WISE
                                 ON POOL_WISE.asset_code = UNTAGED.GL_CODE
                                    AND POOL_WISE.asset_ccy =
                                          UNTAGED.SUKKUKH_CCY)))
GROUP BY GLS.GL_CODE,
         GLS.GL_CODE_DESCRIPTION,
         BAL.AC_CCY,
         gls.FUND_GL_CATEGORY
----



UNION ALL
SELECT 'IJARAH' AS Categorys,
       GLS.GL_CODE || ' ' || BAL.AC_CCY AS GL,
       BAL.AC_CCY CCY,
       GLS.GL_CODE_DESCRIPTION AS GL_DESC,
       SUM (BAL.LCY_AMOUNT) AS ALL_BALANCE_Place
FROM flexcube.IATM_CATEGORY_GL_FUND_LINK GLS,
     flexcube.IATM_CATEGORY_GL_STATISTICS bal
WHERE                                        --GLS.CATEGORY_TYPE = 'ASSET' and
     GLS  .RECORD_STAT = 'O'
      AND GLS.AUTH_STAT = 'A'
      --Missing check. Important to consider only the currently opened maintenance records. Added by: Zeeshan
      AND gls.FUND_GL_CATEGORY = 'BORROWING'
      AND gls.CATEGORY_TYPE = 'LIABILITY'
      AND GLS.START_DATE <= TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      AND NVL (GLS.END_DATE, TO_DATE (:PM_DATE, 'YYYY-MM-DD')) >=
            TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      AND bal.CALC_DATE = TO_DATE (:PM_DATE, 'YYYY-MM-DD')
      /*  (SELECT   MAX (M2.CALC_DATE)
         FROM    flexcube.IATM_CATEGORY_GL_STATISTICS  M2
        WHERE       --bal.AC_BRANCH = M2.AC_BRANCH
                   --AND
                   bal.AC_CCY = M2.AC_CCY
                   AND bal.GL_CODE = M2.GL_CODE
                  and  bal.FUND_GL_CATEGORY=m2.FUND_GL_CATEGORY
                  and bal.CATEGORY_TYPE=m2.CATEGORY_TYPE
                  and m2.BAL_TYPE ='D'
                  AND M2.CALC_DATE <=  TO_DATE (:PM_DATE, 'YYYY-MM-DD') ) */
      AND BAL.ac_branch IN (SELECT BRANCH_CODE
                            FROM flexcube.STTM_BRANCH_ISLAMIC_CUSTOM
                            WHERE IS_LIVE = 'Y')
      --IN (select OFF_CODE from ask_uni.branches_area where flexcube='I')
      AND BAL.AC_CCY = GLS.CURRENCY
      AND GLS.CURRENCY IN ('PKR')
      AND (BAL.GL_cODE IN (SELECT ACCOUNT_HEAD
                           FROM flexcube.cltm_product_rth
                           WHERE accounting_role = 'ACC_DEPR')
           OR (gls.GL_CODE LIKE '32%'                       ---  migration gls
               OR GLS.GL_CODE IN ('30651004', '30651005')))
      AND BAL.gl_code = GLS.GL_CODE
      AND gls.FUND_ID = :PM_FUND_ID
      AND gls.FUND_ID = bal.FUND_ID
      AND gls.currency = bal.AC_CCY
      AND BAL.BAL_TYPE = 'D'
GROUP BY GLS.GL_CODE, GLS.GL_CODE_DESCRIPTION, BAL.AC_CCY