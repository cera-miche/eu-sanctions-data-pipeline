-- SQL-Views_LARGER_v1_2026-07-17.sql
-- Oracle views on a local copy of the UN COMTRADE bulk files.
-- Schema names are replaced with placeholders: MY_SCHEMA (where the
-- views are created) and REF_SCHEMA (HS product-change reference table).
--
-- WHAT CHANGES vs REDUX
--   1. TRANSIT set enlarged: CCA + Turkiye (as before) + Havens (Johannesen
--      2016 list MINUS EU/aligned Europeans) + Western Balkans + China +
--      Iran + India + UAE + Gulf.  34 partner codes, granular groups.
--   2. Window: 2010-01 .. 2026-12 (was 2014-01+). 2010-2011 records are
--      vintage H3 (HS2007) -> PROD_CHANGES filter extended to year 2007.
--   3. Naming prefix LARGER_ (all names <= 29 chars, Oracle limit 30).
--   Everything else replicates the REDUX template verbatim (columns,
--   joins, ISO dates, PARALLEL hint, PROD_CHANGES pattern).
--
-- COUNTRY SETS (codes verified in COMTRADE.REPORTERS / PARTNER_AREA,
-- 2026-07-17)
--   EU27 (Sanctioning, 27):
--     40,56,100,191,196,203,208,233,246,251,276,300,348,372,380,428,440,
--     442,470,528,616,620,642,703,705,724,752
--   Targets: Russia 643, Belarus 112
--   TRANSIT (34):
--     CCA (8):     51,31,268,398,417,762,860,496   [762 TJK never reports;
--                                                   496 MNG reports to 2021]
--     Turkey:      792
--     Havens (11): 44 BHS, 60 BMU, 136 CYM, 530 ANT (dissolved 2010),
--                  531 CUW, 534 SXM, 591 PAN, 48 BHR, 344 HKG, 446 MAC,
--                  702 SGP    [Johannesen p.823 minus AUT/BEL/LUX (EU27)
--                              and CHE/LIE/Guernsey/IoM/Jersey (aligned)]
--     Balkans (6): 8 ALB, 70 BIH, 412 Kosovo (PARTNER-ONLY, never
--                  reports), 499 MNE, 807 MKD, 688 SRB
--     China: 156 | Iran: 364 | India: 699 | UAE: 784
--     Gulf (4): 682 SAU, 634 QAT, 414 KWT, 512 OMN
--
-- BEFORE CREATING: audit leftover objects (a TABLE with a colliding name
-- blocks CREATE OR REPLACE VIEW):
--   SELECT object_name, object_type FROM all_objects
--   WHERE owner='MY_SCHEMA' AND object_name LIKE 'LARGER\_%' ESCAPE '\';
-- Run each CREATE individually (Ctrl+Enter), not F5 on the whole file.

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_EU27_TO_RU_BY AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    'Sanctioning' AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE WHEN b.partnercode = 643 THEN 'Russia'
         WHEN b.partnercode = 112 THEN 'Belarus' END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                         372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND b.partnercode IN (643, 112)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_EU27_FROM_RU_BY AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    'Sanctioning' AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE WHEN b.partnercode = 643 THEN 'Russia'
         WHEN b.partnercode = 112 THEN 'Belarus' END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                         372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND b.partnercode IN (643, 112)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_RU_BY_TO_EU27 AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE WHEN b.reportercode = 643 THEN 'Russia'
         WHEN b.reportercode = 112 THEN 'Belarus' END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    'Sanctioning' AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (643, 112)
  AND b.partnercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                        372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_RU_BY_FROM_EU27 AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE WHEN b.reportercode = 643 THEN 'Russia'
         WHEN b.reportercode = 112 THEN 'Belarus' END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    'Sanctioning' AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (643, 112)
  AND b.partnercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                        372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_EU27_TO_TRANSIT AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    'Sanctioning' AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE
      WHEN b.partnercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.partnercode = 792                               THEN 'Turkey'
      WHEN b.partnercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                             THEN 'Havens'
      WHEN b.partnercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.partnercode = 156 THEN 'China'
      WHEN b.partnercode = 364 THEN 'Iran'
      WHEN b.partnercode = 699 THEN 'India'
      WHEN b.partnercode = 784 THEN 'UAE'
      WHEN b.partnercode IN (682,634,414,512)                THEN 'Gulf'
    END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                         372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND b.partnercode IN (51,31,268,398,417,762,860,496,792,
                        44,60,136,530,531,534,591,48,344,446,702,
                        8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_EU27_FROM_TRANSIT AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    'Sanctioning' AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE
      WHEN b.partnercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.partnercode = 792                               THEN 'Turkey'
      WHEN b.partnercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                             THEN 'Havens'
      WHEN b.partnercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.partnercode = 156 THEN 'China'
      WHEN b.partnercode = 364 THEN 'Iran'
      WHEN b.partnercode = 699 THEN 'India'
      WHEN b.partnercode = 784 THEN 'UAE'
      WHEN b.partnercode IN (682,634,414,512)                THEN 'Gulf'
    END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                         372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND b.partnercode IN (51,31,268,398,417,762,860,496,792,
                        44,60,136,530,531,534,591,48,344,446,702,
                        8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_TRANSIT_TO_EU27 AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE
      WHEN b.reportercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.reportercode = 792                               THEN 'Turkey'
      WHEN b.reportercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                              THEN 'Havens'
      WHEN b.reportercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.reportercode = 156 THEN 'China'
      WHEN b.reportercode = 364 THEN 'Iran'
      WHEN b.reportercode = 699 THEN 'India'
      WHEN b.reportercode = 784 THEN 'UAE'
      WHEN b.reportercode IN (682,634,414,512)                THEN 'Gulf'
    END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    'Sanctioning' AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (51,31,268,398,417,762,860,496,792,
                         44,60,136,530,531,534,591,48,344,446,702,
                         8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND b.partnercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                        372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_TRANSIT_FROM_EU27 AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE
      WHEN b.reportercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.reportercode = 792                               THEN 'Turkey'
      WHEN b.reportercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                              THEN 'Havens'
      WHEN b.reportercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.reportercode = 156 THEN 'China'
      WHEN b.reportercode = 364 THEN 'Iran'
      WHEN b.reportercode = 699 THEN 'India'
      WHEN b.reportercode = 784 THEN 'UAE'
      WHEN b.reportercode IN (682,634,414,512)                THEN 'Gulf'
    END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    'Sanctioning' AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (51,31,268,398,417,762,860,496,792,
                         44,60,136,530,531,534,591,48,344,446,702,
                         8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND b.partnercode IN (40,56,100,191,196,203,208,233,246,251,276,300,348,
                        372,380,428,440,442,470,528,616,620,642,703,705,724,752)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_TRANSIT_TO_RU_BY AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE
      WHEN b.reportercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.reportercode = 792                               THEN 'Turkey'
      WHEN b.reportercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                              THEN 'Havens'
      WHEN b.reportercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.reportercode = 156 THEN 'China'
      WHEN b.reportercode = 364 THEN 'Iran'
      WHEN b.reportercode = 699 THEN 'India'
      WHEN b.reportercode = 784 THEN 'UAE'
      WHEN b.reportercode IN (682,634,414,512)                THEN 'Gulf'
    END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE WHEN b.partnercode = 643 THEN 'Russia'
         WHEN b.partnercode = 112 THEN 'Belarus' END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (51,31,268,398,417,762,860,496,792,
                         44,60,136,530,531,534,591,48,344,446,702,
                         8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND b.partnercode IN (643, 112)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_TRANSIT_FROM_RU_BY AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE
      WHEN b.reportercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.reportercode = 792                               THEN 'Turkey'
      WHEN b.reportercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                              THEN 'Havens'
      WHEN b.reportercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.reportercode = 156 THEN 'China'
      WHEN b.reportercode = 364 THEN 'Iran'
      WHEN b.reportercode = 699 THEN 'India'
      WHEN b.reportercode = 784 THEN 'UAE'
      WHEN b.reportercode IN (682,634,414,512)                THEN 'Gulf'
    END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE WHEN b.partnercode = 643 THEN 'Russia'
         WHEN b.partnercode = 112 THEN 'Belarus' END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (51,31,268,398,417,762,860,496,792,
                         44,60,136,530,531,534,591,48,344,446,702,
                         8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND b.partnercode IN (643, 112)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_EXP_RU_BY_TO_TRANSIT AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE WHEN b.reportercode = 643 THEN 'Russia'
         WHEN b.reportercode = 112 THEN 'Belarus' END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE
      WHEN b.partnercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.partnercode = 792                               THEN 'Turkey'
      WHEN b.partnercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                             THEN 'Havens'
      WHEN b.partnercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.partnercode = 156 THEN 'China'
      WHEN b.partnercode = 364 THEN 'Iran'
      WHEN b.partnercode = 699 THEN 'India'
      WHEN b.partnercode = 784 THEN 'UAE'
      WHEN b.partnercode IN (682,634,414,512)                THEN 'Gulf'
    END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'X'
  AND b.reportercode IN (643, 112)
  AND b.partnercode IN (51,31,268,398,417,762,860,496,792,
                        44,60,136,530,531,534,591,48,344,446,702,
                        8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));

CREATE OR REPLACE VIEW MY_SCHEMA.LARGER_IMP_RU_BY_FROM_TRANSIT AS
SELECT /*+ PARALLEL(b, 15) */
    TO_CHAR(b.period, 'YYYY-MM-DD') AS period,
    b.reportercode, r.text AS reporter_name,
    CASE WHEN b.reportercode = 643 THEN 'Russia'
         WHEN b.reportercode = 112 THEN 'Belarus' END AS reporter_group,
    b.partnercode, p.text AS partner_name,
    CASE
      WHEN b.partnercode IN (51,31,268,398,417,762,860,496) THEN 'CCA'
      WHEN b.partnercode = 792                               THEN 'Turkey'
      WHEN b.partnercode IN (44,60,136,530,531,534,591,48,344,446,702)
                                                             THEN 'Havens'
      WHEN b.partnercode IN (8,70,412,499,807,688)           THEN 'Balkans'
      WHEN b.partnercode = 156 THEN 'China'
      WHEN b.partnercode = 364 THEN 'Iran'
      WHEN b.partnercode = 699 THEN 'India'
      WHEN b.partnercode = 784 THEN 'UAE'
      WHEN b.partnercode IN (682,634,414,512)                THEN 'Gulf'
    END AS partner_group,
    b.flowcode, b.cmdcode, b.classificationcode,
    b.qtyunitcode, b.qty, b.netwgt, b.cifvalue, b.fobvalue, b.primaryvalue,
    b.isreported, b.isaggregate
FROM COMTRADE.BULKFILES b
INNER JOIN COMTRADE.REPORTERS    r ON b.reportercode = r.reportercode
INNER JOIN COMTRADE.PARTNER_AREA p ON b.partnercode  = p.partnercode
WHERE b.period BETWEEN TO_DATE('201001','YYYYMM') AND TO_DATE('202612','YYYYMM')
  AND b.flowcode = 'M'
  AND b.reportercode IN (643, 112)
  AND b.partnercode IN (51,31,268,398,417,762,860,496,792,
                        44,60,136,530,531,534,591,48,344,446,702,
                        8,70,412,499,807,688,156,364,699,784,682,634,414,512)
  AND EXISTS (SELECT 1 FROM REF_SCHEMA.PROD_CHANGES h
              WHERE h.year IN (2007,2012,2017,2022)
                AND SUBSTR(h.product,1,6) = SUBSTR(b.cmdcode,1,6));


-- ============================================================================
-- VERIFICATION QUERIES — run after all 12 CREATEs
-- ============================================================================

-- V0: all twelve views exist
SELECT view_name FROM all_views
WHERE owner = 'MY_SCHEMA' AND view_name LIKE 'LARGER\_%' ESCAPE '\'
ORDER BY view_name;

-- V1 (ANCHOR vs REDUX, must match EXACTLY): Germany -> Russia, Jan 2022
-- REDUX baseline: 40,508
SELECT COUNT(*) FROM MY_SCHEMA.LARGER_EXP_EU27_TO_RU_BY
WHERE reportercode = 276 AND partnercode = 643 AND period LIKE '2022-01%';

-- V2 (ANCHOR vs REDUX, must match EXACTLY): Russia <- Germany, Jan 2021
-- REDUX baseline: 2,448
SELECT COUNT(*) FROM MY_SCHEMA.LARGER_IMP_RU_BY_FROM_EU27
WHERE reportercode = 643 AND partnercode = 276 AND period LIKE '2021-01%';

-- V3 (NEW WINDOW): Germany -> Russia, Jan 2010 (vintage H3 expected, >0)
SELECT classificationcode, COUNT(*) FROM MY_SCHEMA.LARGER_EXP_EU27_TO_RU_BY
WHERE reportercode = 276 AND partnercode = 643 AND period LIKE '2010-01%'
GROUP BY classificationcode;

-- V4 (NEW COUNTRIES, spot): China -> Russia, Jan 2023 (>0 expected)
SELECT COUNT(*) FROM MY_SCHEMA.LARGER_EXP_TRANSIT_TO_RU_BY
WHERE reportercode = 156 AND partnercode = 643 AND period LIKE '2023-01%';

-- V5 (NEW COUNTRIES, spot): Germany -> UAE, Jan 2023 (>0 expected)
SELECT COUNT(*) FROM MY_SCHEMA.LARGER_EXP_EU27_TO_TRANSIT
WHERE reportercode = 276 AND partnercode = 784 AND period LIKE '2023-01%';

-- V6 (GROUP COVERAGE): no NULL groups on a heavy view, one month
SELECT reporter_group, COUNT(*) FROM MY_SCHEMA.LARGER_EXP_TRANSIT_TO_RU_BY
WHERE period LIKE '2023-01%' GROUP BY reporter_group ORDER BY 2 DESC;

-- V7 (EXPORT PLANNING — SLOW, minutes each; run one at a time, smallest
--     leg first): rows per reporter_group per view, to size the slices.
SELECT 'EXP_TRANSIT_TO_RU_BY' v, reporter_group g, COUNT(*) n
FROM MY_SCHEMA.LARGER_EXP_TRANSIT_TO_RU_BY GROUP BY reporter_group
UNION ALL
SELECT 'IMP_TRANSIT_FROM_EU27', reporter_group, COUNT(*)
FROM MY_SCHEMA.LARGER_IMP_TRANSIT_FROM_EU27 GROUP BY reporter_group
ORDER BY 1, 3 DESC;
-- (repeat the pattern for the other heavy views; the EU-reported ones
--  can be counted whole: single reporter_group.)

-- EXPORT STRATEGY REMINDER (G11/G12): never export a heavy view whole.
-- Slice by reporter_group (and by year-blocks for China/India/Turkey):
--   CREATE OR REPLACE VIEW MY_SCHEMA.TMP_L_EXP_CHINA_TO_RU_BY AS
--   SELECT * FROM MY_SCHEMA.LARGER_EXP_TRANSIT_TO_RU_BY
--   WHERE reporter_group = 'China';
-- export, verify row count vs V7, then DROP VIEW. Local disk, ethernet,
-- sleep disabled. CSV naming: LARGER_<view>__<group>[__<years>].csv.
