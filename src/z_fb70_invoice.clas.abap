CLASS z_fb70_invoice DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

    CLASS-DATA : access_token TYPE string .
    CLASS-DATA : xml_file TYPE string .
    CLASS-DATA : template TYPE string .
    TYPES :
      BEGIN OF struct,
        xdp_template TYPE string,
        xml_data     TYPE string,
        form_type    TYPE string,
        form_locale  TYPE string,
        tagged_pdf   TYPE string,
        embed_font   TYPE string,
      END OF struct."

    CLASS-METHODS :
      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check ,

      read_posts
        IMPORTING VALUE(docno) TYPE CHAR10
                  VALUE(year)     TYPE string
                  comcode         TYPE CHAR4


        RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check .

  PROTECTED SECTION.
  PRIVATE SECTION.

    CONSTANTS lc_ads_render TYPE string VALUE '/ads.restapi/v1/adsRender/pdf'.
    CONSTANTS  lv1_url    TYPE string VALUE 'https://adsrestapi-formsprocessing.cfapps.eu10.hana.ondemand.com/v1/adsRender/pdf?templateSource=storageName&TraceLevel=2'  .
    CONSTANTS  lv2_url    TYPE string VALUE 'https://btp-yvzjjpaz.authentication.eu10.hana.ondemand.com/oauth/token'  .
    CONSTANTS lc_storage_name TYPE string VALUE 'templateSource=storageName'.
    CONSTANTS lc_template_name TYPE string VALUE 'CREDIT_DEBIT_FI_ZIRN'.
ENDCLASS.



CLASS Z_FB70_INVOICE IMPLEMENTATION.


  METHOD create_client .
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD .


  METHOD if_oo_adt_classrun~main.
    DATA(xml)  = read_posts( year = '2023' docno = '' comcode = '' )   .
  ENDMETHOD.


 METHOD read_posts .

data add1 TYPE string .
data add2 type string.
data add3 type string.
data add4 type string.
data gst type string.
data cin type string.
data pan type string.
data email TYPE string.
data bankname TYPE string.
data bankacc TYPE string.
data ifsc TYPE string.
data bankbranch TYPE string.
data modeofpayment TYPE string.
DATA sno TYPE  c LENGTH 3.

        SELECT SINGLE * FROM i_operationalacctgdocitem
    WHERE accountingdocument = @docno
       AND fiscalyear = @year
       AND companycode = @comcode  INTO @data(plant2).


  IF plant2-BusinessPlace = '' OR plant2-BusinessPlace IS INITIAL.


   if plant2-Plant = '1100' or  plant2-Plant = '1110' or  plant2-Plant = '1120' or  plant2-Plant = '1130' or  plant2-Plant = '1140' or  plant2-Plant = '1210' or  plant2-Plant = '1220' or  plant2-Plant = '1230'.
      gst = '08AAGCV0492E1ZB' .
      add1 = 'Velnik India Limited' .
      add2 = 'Khasra Nos.456/262, 269, 270, 271, 272, 273, 274, 275, 276/1, 28213, 290, 2912, 291/4, 2941,' .
      add3 = 'Kalab Kalan Road, Dholi Magri Choraha, Gram Kalakot, Tehsil-Raipur, District-Beawar' .
      cin = 'U24100RJ20217PLC058778'.
      pan = 'AAGCV0492E' .
*      add5 = '' .

    elseif plant2-Plant = '1000'.
      gst = '23AAGCV0492E1ZJ' .
      add1 = 'Velnik India Limited' .
      add2  = '516-517, BIJALPUR, NEAR DUTT COLD STORAGE, MUNDI ROAD,' .
      add3  = 'INDORE -452012.' .
      cin  = 'U24100RJ20217PLC058778' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .

    elseif plant2-Plant = '1150'.
      gst = '29AAGCV0492E1Z7' .
      add1 = 'Velnik India Limited' .
      add2  = '1, 2, 3 Floor, 222, Kaveri Complex, J.B. Kaval, Survey No.1,' .
      add3  = 'Rajiv Gandhi Nagar, Nandini Layout Bengaluru.' .
      cin  = '' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .

    ENDIF.

    ELSE.
     IF plant2-BusinessPlace = '1000'.
     gst = '08AAGCV0492E1ZB' .
      add1 = 'Velnik India Limited' .
      add2 = 'Khasra Nos.456/262, 269, 270, 271, 272, 273, 274, 275, 276/1, 28213, 290, 2912, 291/4, 2941,' .
      add3 = 'Kalab Kalan Road, Dholi Magri Choraha, Gram Kalakot, Tehsil-Raipur, District-Beawar' .
      cin = 'U24100RJ20217PLC058778'.
      pan = 'AAGCV0492E' .
*      add5 = '' .


     elseif plant2-BusinessPlace = '1001'.
      gst = '23AAGCV0492E1ZJ' .
      add1 = 'Velnik India Limited' .
      add2  = '516-517, BIJALPUR, NEAR DUTT COLD STORAGE, MUNDI ROAD,' .
      add3  = 'INDORE -452012.' .
      cin  = 'U24100RJ20217PLC058778' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .


     elseif plant2-BusinessPlace = '1002'.
            gst = '29AAGCV0492E1Z7' .
      add1 = 'Velnik India Limited' .
      add2  = '1, 2, 3 Floor, 222, Kaveri Complex, J.B. Kaval, Survey No.1,' .
      add3  = 'Rajiv Gandhi Nagar, Nandini Layout Bengaluru.' .
      cin  = '' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .

           elseif plant2-BusinessPlace = '2000'.
            gst = '23AAGCV0492E1ZJ' .
      add1 = 'Ethica Herbals' .
      add2  = '516-517, BIJALPUR, NEAR DUTT COLD STORAGE, MUNDI ROAD' .
      add3  = 'INDORE -452012,' .
      cin  = 'U24100RJ20217PLC058778' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .

           elseif plant2-BusinessPlace = '3000'.
            gst = '29AAGCV0492E1Z7' .
      add1 = 'SMKDR Pvt. Ltd.' .
      add2  = '1, 2, 3 Floor, 222, Kaveri Complex, J.B. Kaval, Survey No.1,' .
      add3  = 'Rajiv Gandhi Nagar, Nandini Layout Bengaluru.' .
      cin  = '' .
      pan   = 'AAGCV0492E' .
*      add5  = '' .

       elseif plant2-BusinessPlace = '3001'.
      gst = '33AAGCV4391L1ZX' .
      add1 = 'SMKDR Pvt. Ltd.' .
      add2  = 'PLOT NO 286 HIG 4th Block , 1 Street MMDA Colony' .
      add3  = 'Chennai- 600095 Tamil Nadu' .
      cin  = 'U46909MP2018PTC045881' .
      pan   = 'AAGCV4391L' .
*      add5  = '' .

      elseif plant2-BusinessPlace = '3002'.
      gst = '29AAGCV4391L1ZM' .
      add1 = 'SMKDR Pvt. Ltd.' .
      add2  = 'Site No. 123, Sy. No. 114/3 , Yelachaguppe Village, Tavarekere' .
      add3  = 'Bengaluru- 562130 Karnataka' .
      cin  = 'U46909MP2018PTC045881' .
      pan   = 'AAGCV4391L' .
*      add5  = '' .


      ENDIF.
       ENDIF.


         SELECT * FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS as a
       LEFT OUTER JOIN I_PurchaseOrderItemAPI01 WITH PRIVILEGED ACCESS as b ON ( b~PurchaseOrder = a~PurchasingDocument AND b~PurchaseOrderItem = a~PurchasingDocumentItem )
       WHERE accountingdocument = @docno and a~FiscalYear = @year

       and a~CompanyCode = @comcode AND ( ( a~AccountingDocumentItemType = 'M' AND a~Material IS NOT INITIAL )
       OR ( a~AccountingDocumentItemType = ' ' AND a~FinancialAccountType <> 'D' )   ) AND a~TransactionTypeDetermination <> 'WIT'
*       AND (   a~GLAccount <> '0003501006' OR a~GLAccount <> '0004181022' )

       INTO TABLE @DATA(it).

       DELETE it WHERE  (    A-GLAccount = '0003501006' OR a-GLAccount = '0004181022' ) .






 SELECT SINGLE * FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS as a
 LEFT OUTER JOIN I_Customer WITH PRIVILEGED ACCESS as b ON ( b~CUSTOMER = a~CUSTOMER )
 LEFT JOIN i_address_2 WITH PRIVILEGED ACCESS as c ON ( c~AddressID = b~AddressID )
 LEFT OUTER JOIN i_regiontext WITH PRIVILEGED ACCESS as d ON  ( d~region = c~Region AND d~language = 'E' AND d~country = c~country  )
 LEFT OUTER JOIN i_regiontext WITH PRIVILEGED ACCESS as e ON  ( e~region = a~IN_GSTPlaceOfSupply AND e~language = 'E' AND e~country = c~country  )
 LEFT OUTER JOIN I_CountryText WITH PRIVILEGED ACCESS as f ON  ( f~Country = c~Country AND f~language = 'E'  )
 WHERE accountingdocument = @docno and a~FiscalYear = @year
 and a~CompanyCode = @comcode AND a~FinancialAccountType = 'D' INTO  @DATA(wasup).

DATA: documentname TYPE string.

READ TABLE it INTO DATA(wa) INDEX 1.
     IF wa-A-accountingdocumenttype = 'DN'  .
        documentname = 'Debit Note' .
     ELSEIF  wa-A-accountingdocumenttype = 'DG'  .
        documentname = 'Credit Note'  .
     ELSEIF  wa-A-accountingdocumenttype = 'DR'  .
        documentname = 'Invoice'  .
     ENDIF .
 data aginvoice TYPE string.
 data aginvoice_2 TYPE string.
 data indate TYPE string.

 SELECT SINGLE a~Reference1IDByBusinessPartner from I_OperationalAcctgDocItem WITH PRIVILEGED ACCESS as a
 INNER JOIN  I_JournalEntry WITH PRIVILEGED ACCESS AS b on
 ( a~AccountingDocument = b~AccountingDocument and a~FiscalYear = b~FiscalYear and a~CompanyCode = b~CompanyCode AND  b~IsReversal = '' AND b~IsReversed = '' )
 WHERE a~AccountingDocument  = @docno AND a~CompanyCode = @comcode AND a~FiscalYear = @year
 AND a~Reference1IDByBusinessPartner IS NOT INITIAL INTO @data(AgainstInvoice).

SELECT SINGLE a~DocumentReferenceID , b~POSTINGDATE FROM   I_JournalEntry WITH PRIVILEGED ACCESS AS A
LEFT OUTER JOIN I_JournalEntry WITH PRIVILEGED ACCESS as b ON ( B~DocumentReferenceID = a~DocumentReferenceID and b~FiscalYear = a~FiscalYear
AND b~CompanyCode = a~CompanyCode AND ( b~AccountingDocumentType = 'KR' OR b~AccountingDocumentType = 'RE' )
AND  b~IsReversal = '' AND b~IsReversed = '' ) WHERE a~AccountingDocument  = @docno AND a~CompanyCode = @comcode
AND a~FiscalYear = @year AND a~IsReversal = '' AND a~IsReversed = ''
AND  a~DocumentReferenceID IS NOT INITIAL INTO @data(InvoiceReference).



READ TABLE IT INTO DATA(HEADWA) INDEX 1.

SELECT SINGLE a~reversalreferencedocument from i_journalentry WITH PRIVILEGED ACCESS as a
 WHERE a~accountingdocument = @HEADWA-a-AccountingDocument AND a~fiscalyear = @HEADWA-a-fiscalyear
 AND a~fiscalyear = @HEADWA-a-FiscalYear

    INTO  @DATA(reversalreferencedocument).

SELECT SINGLE *  From  I_JournalEntry  WITH PRIVILEGED ACCESS as a
LEFT OUTER JOIN I_JournalEntry WITH PRIVILEGED ACCESS as b on ( a~DOCUMENTREFERENCEID = b~DOCUMENTREFERENCEID  and (   b~AccountingDocumentType = 'RV' OR b~AccountingDocumentType = 'UE'  )    )

WHERE   a~AccountingDocument   = @docno INTO @DATA(DOCUMENTREFERENCEID) .

if wa-A-accountingdocumenttype = 'DG' or wa-A-accountingdocumenttype = 'DN'.


  SELECT SINGLE Documentitemtext FROM I_OperationalAcctgDocItem WITH PRIVILEGED ACCESS as a WHERE AccountingDocument = @HEADWA-a-AccountingDocument
  AND AccountingDocumentItem = @HEADWA-a-AccountingDocumentItem AND FiscalYear = @HEADWA-a-FiscalYear AND AccountingDocumentItemType = '' AND
  FinancialAccountType = 'S' INTO @DATA(Documentitemtext).
*
IF DOCUMENTREFERENCEID-B-DOCUMENTREFERENCEID IS NOT INITIAL.

aginvoice = DOCUMENTREFERENCEID-B-DOCUMENTREFERENCEID.
aginvoice_2 = DOCUMENTREFERENCEID-B-DOCUMENTREFERENCEID.

ELSE.
aginvoice = Documentitemtext.
ENDIF.

SELECT SINGLE CreationDate from I_BillingDocument WITH PRIVILEGED ACCESS WHERE BillingDocument = @aginvoice_2   INTO @data(date).
IF date IS INITIAL .
SELECT SINGLE POSTINGDATE from I_JournalEntry WITH PRIVILEGED ACCESS WHERE DocumentReferenceID = @aginvoice_2   INTO @date.
ENDIF.

if DOCUMENTREFERENCEID-B-DOCUMENTREFERENCEID is NOT INITIAL.
IF date IS NOT INITIAL.
*indate = |{ date+6(2) }/{ date+4(2) }/{ date+0(4) }|.
indate =  |{ DOCUMENTREFERENCEID-B-DocumentDate+6(2) }/{ DOCUMENTREFERENCEID-B-DocumentDate+4(2) }/{ DOCUMENTREFERENCEID-B-DocumentDate+0(4) }|.
ENDIF.
ENDIF.
ELSE.


aginvoice = InvoiceReference-DocumentReferenceID.

IF InvoiceReference-PostingDate IS NOT INITIAL .

indate = InvoiceReference-PostingDate+6(2) / InvoiceReference-PostingDate+4(2) / InvoiceReference-PostingDate+0(4).
ENDIF.
ENDIF.

 SELECT SINGLE * FROM Y1IG_INVREFNUM_DD  WITH PRIVILEGED ACCESS WHERE Bukrs = @comcode and  DocYear = @year AND Docno  = @docno INTO @DATA(QRCode) .

 "SELECT SINGLE WBSElement FROM ZWBSELEMENT_F4 WITH PRIVILEGED ACCESS WHERE WBSElementInternalID = @wa-a-WBSElementInternalID INTO @DATA(WBSElement) .

 DATA: name_Invoice TYPE string.
 DATA: name_InvoiceDate TYPE string.
      IF wa-A-accountingdocumenttype = 'DN'  .
        name_Invoice     = 'Debit Note No' .
        name_InvoiceDate = 'Debit Note Date' .
     ELSEIF  wa-A-accountingdocumenttype = 'DG'  .
        name_Invoice     = 'Credit Note No'  .
        name_InvoiceDate = 'Credit Note Date'  .
     ELSE .
        name_Invoice     = 'Invoice No'  .
        name_InvoiceDate = 'Invoice Date'  .
     ENDIF .

if qrcode-IrnStatus = 'CAN'.
   qrcode-IrnStatus = 'X'.
   ENDIF.


  DATA(lv_xml) =
      |<form1>| &&
      |<FORMTYPE>| &&
      |<FORMTYPE>{ documentname }</FORMTYPE>| &&
      |<comcode>{ comcode }</comcode>| &&
      |</FORMTYPE>| &&
      "|<PurchaseOrder>: { WBSElement+0(4) } </PurchaseOrder>| &&
      |<InvoiceNo_name>{ name_Invoice } </InvoiceNo_name>| &&
      |<InvoiceNo>: { docno } </InvoiceNo>| &&

      |<InvoiceDate_name>{ name_InvoiceDate }</InvoiceDate_name>| &&
      |<InvoiceDate>: { wa-a-PostingDate+6(2) }/{ wa-a-PostingDate+4(2) }/{ wa-a-PostingDate+0(4) }</InvoiceDate>| &&
      |<SupplierAddress>{ wasup-c-addresseefullname }</SupplierAddress>| &&
      |<SupplierAddress1>{ wasup-c-street }{ wasup-c-streetname } { wasup-c-streetprefixname1 } </SupplierAddress1>| &&
      |<SupplierAddress2>{ wasup-c-cityname }</SupplierAddress2>| &&
      |<SupplierAddress3>{ wasup-c-postalcode } , { wasup-f-CountryName }</SupplierAddress3>| &&
      |<PlaceofSupply>: { wasup-a-IN_GSTPlaceOfSupply } ( { wasup-e-RegionName } )</PlaceofSupply>| &&
      |<UNTIADD>{ wa-a-ProfitCenter }</UNTIADD>| &&
      |<StateCode>: { wasup-c-region }( { wasup-d-RegionName } )</StateCode>| &&
      |<GSTNNo>: { wasup-b-TaxNumber3 }</GSTNNo>| &&
      |<PANNo>: { wasup-b-TaxNumber3+2(10) }</PANNo>| &&
*      |<AgainstInvoice>:  { aginvoice }</AgainstInvoice>| &&
      |<AgainstInvoice>{ aginvoice }</AgainstInvoice>| &&
      |<InvoiceDate>: { indate }</InvoiceDate>| &&
      |<IRN>: { QRCode-AckNo }</IRN>| &&
      |<IRNDATE>: { QRCode-AckDate }</IRNDATE>| &&
      |<QRCodeBarcode1>{ QRCode-SignedQrcode }</QRCodeBarcode1>| &&
      |<gstin>{ gst }</gstin>| &&
      |<CINno>{ cin }</CINno>| &&
      |<panno>{ pan }</panno>| &&
      |<tanno></tanno>| &&
      |<state_code>08</state_code>| &&
      |<ReverseDocumentNO>{ ReversalReferenceDocument }</ReverseDocumentNO>| &&
      |<IsReversal>{ qrcode-IrnStatus }</IsReversal>| &&
      |<Table2>| &&
      |<HeaderRow/>| .

DATA TEXT TYPE STRING.
DATA TEXT1 TYPE STRING.
DATA TOTALBASIC TYPE P DECIMALS 2.
DATA TOTALBASIC1 TYPE P DECIMALS 2.

LOOP AT IT INTO DATA(WAITEM) WHERE    A-GLAccount <> '4451001050'   .

*  SELECT SINGLE ProductDescription FROM I_ProductDescriptionTP_2 WITH PRIVILEGED ACCESS as a WHERE Product = @waitem-a-Product
*  AND Language = 'E' INTO @TEXT.

"15.06.2025
*  SELECT SINGLE Documentitemtext FROM I_OperationalAcctgDocItem WITH PRIVILEGED ACCESS as a WHERE AccountingDocument = @waitem-a-AccountingDocument
*  AND AccountingDocumentItem = @waitem-a-AccountingDocumentItem AND FiscalYear = @waitem-a-FiscalYear AND AccountingDocumentItemType = '' AND
*  FinancialAccountType = 'S' INTO @TEXT.


  DATA N TYPE I .

  TEXT1 = waitem-a-Product.

*  IF TEXT IS INITIAL .

  SELECT SINGLE GLAccountName FROM I_GLAccountText WITH PRIVILEGED ACCESS as a WHERE GLAccount =  @waitem-a-GLAccount
  AND Language = 'E' INTO @TEXT.

  TEXT1 =  waitem-a-GLAccount .
*
*  ENDIF.
  TOTALBASIC1 = TOTALBASIC1 +  waitem-a-AmountInCompanyCodeCurrency.
  IF waitem-a-AmountInCompanyCodeCurrency < 0 .
  waitem-a-AmountInCompanyCodeCurrency = waitem-a-AmountInCompanyCodeCurrency * -1.
  ENDIF.

IF waitem-a-GLAccount <> '0004651011'.
  TOTALBASIC = TOTALBASIC + waitem-a-AmountInCompanyCodeCurrency.
  ENDIF.

   N = N + 1.
  lv_xml = lv_xml &&

         |<Row1>| &&
         |<PoSlNo>{ N }</PoSlNo>| &&
         |<material>{ waitem-a-Product }</material>| &&
         |<Description>{ TEXT }</Description>| &&
         |<HSNSACCodeDescription>{ waitem-a-IN_HSNOrSACCode }</HSNSACCodeDescription>| &&
         |<Quantityuom>{ waitem-a-Quantity }</Quantityuom>| &&
         |<UOM>{ waitem-a-BaseUnit }</UOM>| &&
*         |<RateINR>{ waitem-b-NetPriceAmount }</RateINR>| &&
         |<Amount>{ waitem-a-AmountInCompanyCodeCurrency }</Amount>| &&
         |</Row1>| .
    CLEAR:TEXT1,TEXT.
ENDLOOP.

SELECT siNGLE SUM( CASE WHEN TransactionTypeDetermination  = 'JOC' THEN  AmountInCompanyCodeCurrency else 0  END ) as CSGST,
                SUM( CASE
          WHEN TransactionTypeDetermination = 'JOI'
          THEN AmountInCompanyCodeCurrency
          ELSE 0
       END ) AS IGST,
*              CASE WHEN TransactionTypeDetermination  = 'JOI' THEN SUM( AmountInCompanyCodeCurrency ) END as IGST,
            SUM(  CASE WHEN TransactionTypeDetermination  = 'WIT' THEN  AmountInCompanyCodeCurrency else 0 END ) as TDS

 FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS  WHERE  accountingdocument = @docno and FiscalYear = @year
       and CompanyCode = @comcode AND ( TransactionTypeDetermination  = 'JOC' OR TransactionTypeDetermination  = 'JOI' OR TransactionTypeDetermination = 'WIT' )
 INTO @DATA(TAX1).

SELECT siNGLE SUM( CASE WHEN TransactionTypeDetermination  = 'JOC' THEN  AmountInCompanyCodeCurrency else 0  END ) as CSGST,
                SUM( CASE
          WHEN TransactionTypeDetermination = 'JOI'
          THEN AmountInCompanyCodeCurrency
          ELSE 0
       END ) AS IGST,
*              CASE WHEN TransactionTypeDetermination  = 'JOI' THEN SUM( AmountInCompanyCodeCurrency ) END as IGST,
            SUM(  CASE WHEN TransactionTypeDetermination  = 'WIT' THEN  AmountInCompanyCodeCurrency else 0 END ) as TDS,
              TAXCODE
 FROM i_operationalacctgdocitem WITH PRIVILEGED ACCESS  WHERE  accountingdocument = @docno and FiscalYear = @year
       and CompanyCode = @comcode AND ( TransactionTypeDetermination  = 'JOC' OR TransactionTypeDetermination  = 'JOI' OR TransactionTypeDetermination = 'WIT' )
       GROUP BY TAXCODE INTO @DATA(TAX).

SELECT SINGLE gstrate FROM ztax_code_table WITH PRIVILEGED ACCESS as a WHERE taxcode =  @TAX-TAXCODE INTO @DATA(gstrate).
DATA RateofGST TYPE STRING.


DATA TOTGST TYPE P DECIMALS 2.
DATA NetAmount TYPE P DECIMALS 2.
DATA NetAmount1 TYPE  p DECIMALS 2.



SELECT SINGLE SUM( AmountInCompanyCodeCurrency ) as round FROM i_operationalacctgdocitem as a
WHERE  accountingdocument = @docno and FiscalYear = @year
       and CompanyCode = @comcode AND ( a~GLAccount = '0004651011'  ) INTO  @DATA(ROUNDAMT).

IF TAX-igst <> 0 .
RateofGST = | IGST | && gstrate && | (%) | .
ELSEIF TAX-CSGST <> 0 .
IF gstrate <> 0 .
gstrate = gstrate  .
ENDIF.
RateofGST = | SGST | &&  gstrate && | (%) | && | / | && | SGST | && gstrate && | (%) |.
ENDIF.


NetAmount1 = TOTALBASIC1 + TAX1-igst + ( 2 * TAX1-csgst ) .



*NetAmount  =  TOTALBASIC + TAX-igst + ( 2 * TAX-csgst ).
TOTGST     =   TAX1-igst + ( 2 * TAX1-csgst ) .
IF NetAmount1 < 0 .
NetAmount1 = NetAmount1 * - 1.
ENDIF.

IF roundamt < 0 .
roundamt = roundamt * - 1 .
ENDIF.

IF TAX-igst < 0 .
TAX1-igst = TAX1-igst * - 1 .
ENDIF.
IF TAX-csgst < 0 .
TAX1-csgst = TAX1-csgst * - 1.
ENDIF.

IF TOTGST < 0 .
TOTGST = TOTGST * - 1.
ENDIF.

     SELECT SINGLE B~UserDescription FROM I_JournalEntry AS A
  LEFT OUTER JOIN I_User WITH PRIVILEGED ACCESS AS B  ON ( B~UserID = A~AccountingDocCreatedByUser )
     WHERE A~AccountingDocument = @docno AND A~FiscalYear = @year   INTO @DATA(UserDescription).

  lv_xml = lv_xml &&
       |</Table2>| &&
       |<Basic>{ TOTALBASIC }</Basic>| &&
       |<IGSTpercent></IGSTpercent>| &&
       |<IGSTAMOUNT>{ TAX1-igst  }</IGSTAMOUNT>| &&
       |<CGSTpercent> (%)</CGSTpercent>| &&
       |<CGSTAMOUNT>{ TAX-csgst }</CGSTAMOUNT>| &&
       |<SGSTpercent> (%)</SGSTpercent>| &&
       |<SGSTAMOUNT>{ TAX-csgst  }</SGSTAMOUNT>| &&
       |<Roundedoff1> { ROUNDAMT } </Roundedoff1>| &&
       |<GrossTotal>{ NetAmount1 }</GrossTotal>| &&
       |<remark>{ wasup-a-DocumentItemText }</remark>| &&
       |<AMOUNT>| &&
       |<Totaltaxablevalue>{  TOTALBASIC  }</Totaltaxablevalue>| &&
       |<RateofGST>{ RateofGST }</RateofGST>| &&
       |<GSTPayable>{ TOTGST }</GSTPayable>| &&
       |<TotalAMTWORD></TotalAMTWORD>| &&
       |</AMOUNT>| &&
       |<GST>| &&
       |<GSTlAMTWORD></GSTlAMTWORD>| &&
       |<Preparedby1>{ UserDescription }</Preparedby1>| &&
       |</GST>| &&
       |<add1>{ add1 }</add1>| &&
       |<add2>{ add2 }</add2>| &&
       |<add3>{ add3 }</add3>| &&
       |<add4>{ add4 }</add4>| &&
       |<E-MAIL>{ email }</E-MAIL>| &&
       |<E-MAIL2></E-MAIL2>| &&
       |<PHONE></PHONE>| &&
       |<FAX></FAX>| &&
       |<authsign>{ add1 }</authsign>| &&
       |</form1>| .

       CALL METHOD zadobe_print=>adobe(
       EXPORTING
        xml  = lv_xml
         form_name = lc_template_name
      RECEIVING
        result   = result12 ).
   ENDMETHOD.
ENDCLASS.
