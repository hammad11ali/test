
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