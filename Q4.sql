
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
