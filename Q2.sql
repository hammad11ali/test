
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
