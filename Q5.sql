
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
