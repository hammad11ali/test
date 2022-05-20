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
