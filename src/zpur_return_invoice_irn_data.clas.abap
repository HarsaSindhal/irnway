CLASS zpur_return_invoice_irn_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun .
    CLASS-DATA : distan TYPE string .

    CLASS-DATA:BEGIN OF buyerdtls,
                 gstin(30) TYPE c,
                 lglnm     TYPE string,
                 trdnm     TYPE string,
                 addr1     TYPE string,
                 addr2     TYPE string,
                 loc       TYPE string,
                 pin       TYPE string,
                 stcd      TYPE string,
               END OF buyerdtls.

    CLASS-DATA:BEGIN OF sellerdtls,
                 gstin(30) TYPE c,
                 lglnm     TYPE string,
                 trdnm     TYPE string,
                 addr1     TYPE string,
                 addr2     TYPE string,
                 loc       TYPE string,
                 pin       TYPE string,
                 stcd      TYPE string,
               END OF sellerdtls.

    CLASS-DATA:BEGIN OF dispdtls,
                 nm    TYPE string,
                 addr1 TYPE string,
                 addr2 TYPE string,
                 loc   TYPE string,
                 pin   TYPE string,
                 stcd  TYPE string,
               END OF dispdtls.

    CLASS-DATA:BEGIN OF expshipdtls,
                 lglnm TYPE string,
                 addr1 TYPE string,
                 loc   TYPE string,
                 pin   TYPE string,
                 stcd  TYPE string,
               END OF expshipdtls.

    TYPES :BEGIN OF ty_itemlist,
             prodname     TYPE string,
             proddesc     TYPE string,
             hsncd        TYPE string,
             qty          TYPE yinv_stu-igstamt,
             unit         TYPE string,
             assamt       TYPE yinv_stu-igstamt,

             cgstrt       TYPE yinv_stu-igstamt,
             cgstamt      TYPE yinv_stu-igstamt,
             sgstrt       TYPE yinv_stu-igstamt,
             sgstamt      TYPE yinv_stu-igstamt,
             igstrt       TYPE yinv_stu-igstamt,
             igstamt      TYPE yinv_stu-igstamt,

             cesrt        TYPE yinv_stu-igstamt,
             cesamt       TYPE yinv_stu-igstamt,
             othchrg      TYPE yinv_stu-igstamt,
             cesnonadvamt TYPE yinv_stu-igstamt,
           END OF ty_itemlist.

    CLASS-DATA:itemlist    TYPE TABLE OF ty_itemlist,
               wa_itemlist LIKE LINE OF itemlist.

    TYPES:BEGIN OF ty_final,
            documentnumber(10)      TYPE c,
            documenttype(10)        TYPE c,
            documentdate(10)        TYPE c,
            supplytype(10)          TYPE c,
            subsupplytype(30)       TYPE c,
            subsupplytypedesc(30)   TYPE c,
            transactiontype(20)     TYPE c,

            buyerdtls               LIKE buyerdtls,
            sellerdtls              LIKE sellerdtls,
            expshipdtls             LIKE expshipdtls,
            dispdtls                LIKE dispdtls,
            itemlist                LIKE itemlist,

            totalinvoiceamount      TYPE yinv_stu-igstamt,
            totalcgstamount         TYPE yinv_stu-igstamt,
            totalsgstamount         TYPE yinv_stu-igstamt,
            totaligstamount         TYPE yinv_stu-igstamt,
            totalcessamount         TYPE yinv_stu-igstamt,
            totalcessnonadvolamount TYPE yinv_stu-igstamt,
            totalassessableamount   TYPE yinv_stu-igstamt,
            otheramount             TYPE yinv_stu-igstamt,
            othertcsamount          TYPE yinv_stu-igstamt,
            transid                 TYPE string,
            transname               TYPE string,
            transmode               TYPE string,
            distance                TYPE string,
            transdocno              TYPE string,
            transdocdt              TYPE string,
            vehno                   TYPE string,
            vehtype                 TYPE string,
          END OF ty_final.

    CLASS-DATA:it_final TYPE TABLE OF ty_final,
               wa_final TYPE ty_final.

    CLASS-METHODS :generated_eway_bill
      IMPORTING VALUE(invoice)     TYPE CHAR10
                  VALUE(companycode) TYPE CHAR4
                  VALUE(year)        TYPE string
                  VALUE(invoice1)     TYPE CHAR14
                transpoter_name TYPE string OPTIONAL
                transportid     TYPE string OPTIONAL
                transportdoc    TYPE string OPTIONAL
                vehiclenumber   TYPE string OPTIONAL
                Eway_generate   TYPE string OPTIONAL
                distance        TYPE string OPTIONAL
                RETURNING VALUE(result)     TYPE string.

    CLASS-DATA:user_name TYPE string,
               password  TYPE string,
               gstin_d   TYPE string.


    CLASS-DATA: BEGIN OF i_eway,
                  ewbnumber      TYPE string,
                  fromplace      TYPE string, "C length 100,
                  fromstate      TYPE string, "N length 2,
                  reasoncode     TYPE string,
                  reasonremark   TYPE string,
                  transdocno     TYPE string,
                  transdocdt     TYPE string,
                  transmode      TYPE string,
                  documentnumber TYPE string,
                  documenttype   TYPE string,
                  documentdate   TYPE string,
                  vehicletype    TYPE string,
                  vehno          TYPE string,
                  updateddate    TYPE string,
                  validupto      TYPE string,
                  errors         TYPE string,
                END OF  i_eway.

    CLASS-DATA:BEGIN OF w_user,
                 user_id  TYPE string,
                 password TYPE string,
                 gstin    TYPE string,
               END OF w_user.

    CLASS-DATA : access_token TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZPUR_RETURN_INVOICE_IRN_DATA IMPLEMENTATION.


 METHOD generated_eway_bill.


    DATA: vbeln1 TYPE c LENGTH 10.


    vbeln1  =    |{ invoice ALPHA = IN }|.


select * from i_operationalacctgdocitem
where LEFT( OriginalReferenceDocument,10 ) = @invoice AND TaxItemAcctgDocItemRef is not INITIAL and AccountingDocumentItemType <> 'T' and FiscalYear = @year
AND CompanyCode = @companycode AND GLAccount <> '1850001003' into table @data(i_billingpart).

**Plant ADdress
select SINGLE ProfitCenter from i_operationalacctgdocitem
where LEFT( OriginalReferenceDocument,10 ) = @invoice   AND ProfitCenter <> '' AND FiscalYear = @year AND CompanyCode = @companycode INTO @DATA(ProfitCenter2) .

*                                                     accountingdocument = @vbeln1
  SELECT SINGLE *  FROM i_operationalacctgdocitem  WHERE LEFT( OriginalReferenceDocument,10 ) = @invoice1  and FiscalYear = @year
  AND CompanyCode = @companycode AND Supplier IS NOT INITIAL INTO   @DATA(i_billingpart1).
*                                                 accountingdocument = @vbeln1
  SELECT *  FROM i_operationalacctgdocitem  WHERE LEFT( OriginalReferenceDocument,10 ) = @invoice1 and FiscalYear = @year
  AND CompanyCode = @companycode AND ( transactiontypedetermination = 'RE' OR transactiontypedetermination = 'KG' ) INTO TABLE  @DATA(maintab).

IF i_billingpart IS  INITIAL .

select * from i_operationalacctgdocitem
where AccountingDocument = @invoice1 AND TaxItemAcctgDocItemRef is not INITIAL and AccountingDocumentItemType <> 'T' and FiscalYear = @year
AND CompanyCode = @companycode AND GLAccount <> '1850001003' into table @i_billingpart.

**Plant ADdress
select SINGLE ProfitCenter from i_operationalacctgdocitem
where AccountingDocument = @invoice1   AND ProfitCenter <> ''
AND FiscalYear = @year AND CompanyCode = @companycode INTO @ProfitCenter2 .

*                                                     accountingdocument = @vbeln1
  SELECT SINGLE *  FROM i_operationalacctgdocitem  WHERE AccountingDocument = @invoice1  and FiscalYear = @year
  AND CompanyCode = @companycode AND Supplier IS NOT INITIAL INTO   @i_billingpart1.
*                                                 accountingdocument = @vbeln1
  SELECT *  FROM i_operationalacctgdocitem  WHERE AccountingDocument = @invoice1 and FiscalYear = @year
  AND CompanyCode = @companycode AND ( transactiontypedetermination = 'RE' OR transactiontypedetermination = 'KG' ) INTO TABLE  @maintab.
ENDIF.


*accountingdocument
DATA PLANT12 TYPE C LENGTH 4.
  plant12    = |{ profitcenter2 ALPHA = OUT }|.

        SELECT SINGLE a~addressid,
                  b~addresssearchterm2 AS gstin,
                  b~careofname AS lglnm,
                  b~AddresseeFullName as Name,
                  b~streetname AS addr1,
                  b~cityname AS addr2,
                  b~districtname AS city,
                  b~postalcode AS pin,
                  b~region AS stcd,
                  a~PLANTNAME,
                  b~AddresseeFullName
    FROM i_plant AS a
    LEFT JOIN i_address_2 WITH PRIVILEGED ACCESS AS b ON ( a~addressid = b~addressid )
    WHERE plant = @plant12 INTO @DATA(sellerplantaddress).


*Billing partener "BUYER'
    SELECT SINGLE * FROM i_Supplier as b
    LEFT JOIN  i_address_2 WITH PRIVILEGED ACCESS AS c ON   ( c~addressid = b~addressid  )
    LEFT JOIN i_addrcurdfltmobilephonenumber AS d ON ( d~addressid = b~addressid  ) WHERE b~Supplier = @i_billingpart1-Supplier  INTO @DATA(buyeradd).

    wa_final-documentnumber    = invoice.
    wa_final-supplytype        = 'I'.
    wa_final-subsupplytypedesc = 'Purchase Return'  .
    wa_final-documenttype      = 'OTH'.
    wa_final-subsupplytype     = '8'.
    wa_final-documentdate  = |{ i_billingpart1-PostingDate+6(2) }/{ i_billingpart1-PostingDate+4(2) }/{ i_billingpart1-PostingDate+0(4) }|.

    IF sellerplantaddress-stcd = 'RJ'.
       sellerplantaddress-stcd = '08'.
    elseif sellerplantaddress-stcd = 'GJ'.
       sellerplantaddress-stcd = '24'.
    ENDIF.

    wa_final-sellerdtls-gstin    =  buyeradd-b-taxnumber3.
    wa_final-sellerdtls-lglnm    = |{ buyeradd-c-organizationname1 } { buyeradd-c-organizationname2 } { buyeradd-c-organizationname3 } { buyeradd-c-organizationname4 }|.
    wa_final-sellerdtls-trdnm    = |{ buyeradd-c-organizationname1 } { buyeradd-c-organizationname2 } { buyeradd-c-organizationname3 } { buyeradd-c-organizationname4 }|.
    wa_final-sellerdtls-addr1    = |{ buyeradd-c-organizationname1 } { buyeradd-c-organizationname2 } { buyeradd-c-organizationname3 } { buyeradd-c-organizationname4 }|.
    wa_final-sellerdtls-addr2    = ''.
    wa_final-sellerdtls-loc      =  buyeradd-c-cityname  .
    wa_final-sellerdtls-stcd     =   buyeradd-c-region .
    wa_final-sellerdtls-pin      = buyeradd-c-postalcode.


    REPLACE ALL OCCURRENCES OF ',' IN wa_final-sellerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-sellerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-sellerdtls-addr2 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-sellerdtls-addr2 WITH ' ' .


 IF sy-SYSID = 'JPX' OR  sy-SYSID = 'KH4' .

    wa_final-buyerdtls-gstin = '08AAFCD5862R018'  .
    ELSE.

     if plant12 = '3201' .
      wa_final-buyerdtls-gstin    =  '08AAMCS8050K1ZT'.

     ELSEIF plant12 = '1101' OR plant12 = '1103'.

     wa_final-buyerdtls-gstin    =  '08ABHCS8575K1ZI'.
     ENDIF.

    ENDIF.
    wa_final-buyerdtls-lglnm = sellerplantaddress-lglnm."buyeradd-b-customername.
    wa_final-buyerdtls-trdnm = sellerplantaddress-lglnm." buyeradd-b-customername.
    wa_final-buyerdtls-addr1 =  sellerplantaddress-name."buyeradd-b-customerfullname.
    wa_final-buyerdtls-addr2 =  sellerplantaddress-addr1 .
    wa_final-buyerdtls-loc   = sellerplantaddress-addr2 .
    wa_final-buyerdtls-pin   = sellerplantaddress-pin   .
    wa_final-buyerdtls-stcd  =  sellerplantaddress-stcd  .

    REPLACE ALL OCCURRENCES OF ',' IN wa_final-buyerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-buyerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-buyerdtls-addr2 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-buyerdtls-addr2 WITH ' ' .

  IF   wa_final-buyerdtls-pin <> wa_final-sellerdtls-pin.
      wa_final-transactiontype   = '2'.
    ELSE.
      wa_final-transactiontype   = '1'.
    ENDIF.

    "ItemList
*    LOOP AT  billing_item INTO DATA(bill_item) .
    LOOP AT  i_billingpart INTO DATA(bill_item) .


     wa_itemlist-prodname    =  bill_item-material .
     wa_itemlist-proddesc    =  bill_item-material .

      SELECT SINGLE * FROM i_productplantbasic AS a LEFT JOIN i_product AS b ON ( a~product = b~product )
       WHERE a~product = @bill_item-material AND a~plant = @bill_item-plant  INTO @DATA(hsnsac).

        wa_itemlist-hsncd      =  hsnsac-a-consumptiontaxctrlcode .
*        wa_final-     =  hsnsac-a-consumptiontaxctrlcode .


      wa_itemlist-qty        =  bill_item-Quantity .
      IF bill_item-BaseUnit           = 'TO' .
        wa_itemlist-unit       =  'TON' .
      ELSEIF bill_item-BaseUnit      = 'MTR' .
        wa_itemlist-unit       =  'MTR' .
      ELSEIF bill_item-BaseUnit     = 'KG' .
        wa_itemlist-unit       =  'KGS' .
        ELSEIF bill_item-BaseUnit     = 'PC' OR  bill_item-BaseUnit     = 'ST' .
        wa_itemlist-unit       =  'NOS' .
      ELSEIF bill_item-BaseUnit+0(3) = 'BAG' .
        wa_itemlist-unit       =  'BAGS'.
        ELSEIF bill_item-BaseUnit+0(3) = 'ZST' .
        wa_itemlist-unit       =  'SET'.
      ELSE .
        wa_itemlist-unit        =  'OTH'.
      ENDIF .


      if bill_item-AmountInCompanyCodeCurrency LT 0 .
      wa_itemlist-assamt      =  ( -1 * bill_item-AmountInCompanyCodeCurrency ).
      else.
      wa_itemlist-assamt      =  bill_item-AmountInCompanyCodeCurrency.
      endif.
      wa_itemlist-cesnonadvamt = '0'.




      """""""""""""""""""""""""""""""""""""""""""""""TAX DATA*******************************

select  single AmountInCompanyCodeCurrency, TaxCode from i_operationalacctgdocitem
where accountingdocument = @bill_item-AccountingDocument AND TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef and
                                                             AccountingDocumentItemType = 'T'
                                                         and ( TransactionTypeDetermination = 'JOC' OR TransactionTypeDetermination = 'JIC' )
                                                         AND CompanyCode = @companycode and FiscalYear = @bill_item-FiscalYear
into  @data(cgst1).
if sy-subcs = 0 .
   SELECT SINGLE * FROM ztax_code_table WHERE taxcode = @cgst1-TaxCode  INTO @DATA(cgstTAXRATE)  .
 wa_itemlist-cgstrt   = cgstTAXRATE-gstrate / cgsttaxrate-status.
 DATA cgstamt  TYPE yinv_stu-igstamt.
 if cgst1-AmountInCompanyCodeCurrency LT 0.
 cgstamt  =   ( -1 * cgst1-AmountInCompanyCodeCurrency ).
 else.
 cgstamt =   cgst1-AmountInCompanyCodeCurrency.
 endif.
ENDIF.


select single AmountInCompanyCodeCurrency , TaxCode from i_operationalacctgdocitem
where accountingdocument = @bill_item-AccountingDocument AND TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef and
                                                             AccountingDocumentItemType = 'T' and
                                                             ( TransactionTypeDetermination = 'JOS' OR TransactionTypeDetermination = 'JIS' )
                                                             AND CompanyCode = @companycode and FiscalYear = @bill_item-FiscalYear
into  @data(sgst1).
if sy-subcs = 0 .
   SELECT SINGLE * FROM ztax_code_table WHERE taxcode = @sgst1-TaxCode  INTO @DATA(sgstTAXRATE)  .
 wa_itemlist-sgstrt   = sgstTAXRATE-gstrate / Sgsttaxrate-status.
 DATA sgstamt  TYPE yinv_stu-igstamt.
 if sgst1-AmountInCompanyCodeCurrency LT 0.
 sgstamt  =   ( -1 * sgst1-AmountInCompanyCodeCurrency ).
 else.
 sgstamt =   sgst1-AmountInCompanyCodeCurrency.
 endif.
ENDIF.

select  single AmountInCompanyCodeCurrency ,TaxCode from i_operationalacctgdocitem
where accountingdocument = @bill_item-AccountingDocument AND TaxItemAcctgDocItemRef = @bill_item-TaxItemAcctgDocItemRef and
                           AccountingDocumentItemType = 'T' and ( TransactionTypeDetermination = 'JOI' OR TransactionTypeDetermination = 'JII' )
                          AND CompanyCode = @companycode and FiscalYear = @bill_item-FiscalYear into  @data(igst1).
if sy-subcs = 0 .
   SELECT SINGLE * FROM ztax_code_table WHERE taxcode = @igst1-TaxCode  INTO @DATA(igstTAXRATE)  .
 wa_itemlist-igstrt   = igstTAXRATE-gstrate .
 DATA igstamt  TYPE yinv_stu-igstamt.
 if igst1-AmountInCompanyCodeCurrency LT 0.
 igstamt  =   ( -1 * igst1-AmountInCompanyCodeCurrency ).
 else.
 igstamt =   igst1-AmountInCompanyCodeCurrency.
 endif.
ENDIF.

      "     wa_itemlist-othchrg              = '0'."otchg  * bill_head-accountingexchangerate .
      wa_final-totalassessableamount              = wa_final-totalassessableamount + wa_itemlist-assamt.
      wa_final-totalcgstamount               = wa_final-totalcgstamount + cgstamt .
      wa_final-totalsgstamount               = wa_final-totalsgstamount    + sgstamt .
      wa_final-totaligstamount               = wa_final-totaligstamount + igstamt .
      wa_final-otheramount             = '0'."wa_final-otheramount + wa_itemlist-othchrg.
      wa_final-totalinvoiceamount             = wa_final-totalinvoiceamount + + wa_itemlist-assamt +
                                         igstamt + cgstamt +
                                         sgstamt.
      IF wa_itemlist-cgstrt IS INITIAL.
        wa_itemlist-cgstrt = '0'.
      ENDIF.
      IF wa_itemlist-sgstrt IS INITIAL.
        wa_itemlist-sgstrt = '0'.
      ENDIF.
      IF wa_itemlist-igstrt IS INITIAL.
        wa_itemlist-igstrt = '0'.
      ENDIF.
      IF wa_itemlist-cesrt IS INITIAL.
        wa_itemlist-cesrt = '0'.
      ENDIF.

      IF wa_itemlist-cesnonadvamt IS INITIAL.
        wa_itemlist-cesnonadvamt = '0'.
      ENDIF.

      APPEND wa_itemlist TO itemlist .
      CLEAR :  wa_itemlist.",materialdese.
      CLEAR : bill_item,hsnsac,sgstamt,igstamt,cgstamt.

    ENDLOOP .


    IF wa_final-totalinvoiceamount IS INITIAL.
      wa_final-totalinvoiceamount = '0'.
    ENDIF.
    IF wa_final-totalcgstamount IS INITIAL.
      wa_final-totalcgstamount = '0'.
    ENDIF.
    IF wa_final-totalsgstamount IS INITIAL.
      wa_final-totalsgstamount = '0'.
    ENDIF.
    IF wa_final-totaligstamount IS INITIAL.
      wa_final-totaligstamount = '0'.
    ENDIF.
    IF wa_final-totalcessamount IS INITIAL.
      wa_final-totalcessamount = '0'.
    ENDIF.
    IF wa_final-totalcessnonadvolamount IS INITIAL.
      wa_final-totalcessnonadvolamount = '0'.
    ENDIF.
    IF wa_final-totalassessableamount IS INITIAL.
      wa_final-totalassessableamount = '0'.
    ENDIF.
    IF wa_final-otheramount IS INITIAL.
      wa_final-otheramount = '0'.
    ENDIF.
    IF wa_final-othertcsamount IS INITIAL.
      wa_final-othertcsamount = '0'.
    ENDIF.

 wa_final-itemlist   = itemlist .

    wa_final-transmode  = '1'. "|{ bill_head-YY1_PreCarrierByTransp_BDH ALPHA = OUT }|.
    wa_final-distance   = 0       .
    wa_final-transname  = transpoter_name .
    wa_final-transid    = transportid     .
    wa_final-transdocno     = transportdoc   .
    wa_final-transdocdt   = wa_final-documentdate    .
    wa_final-vehno      = vehiclenumber .
    wa_final-vehtype    = 'R'  .

    APPEND wa_final TO it_final.


    DATA:json TYPE REF TO if_xco_cp_json_data.

    xco_cp_json=>data->from_abap(
      EXPORTING
        ia_abap      = wa_final
      RECEIVING
        ro_json_data = json ).
    json->to_string(
      RECEIVING
        rv_string = DATA(lv_string) ).

  REPLACE ALL OCCURRENCES OF 'DOCUMENTNUMBER'           IN lv_string WITH 'DocumentNumber' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'DOCUMENTTYPE'             IN lv_string WITH 'DocumentType'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'DOCUMENTDATE'             IN lv_string WITH 'DocumentDate'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SUPPLYTYPE'               IN lv_string WITH 'SupplyType'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SUBSUPPLYTYPE'            IN lv_string WITH 'SubSupplyType'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SUBSupplyType'            IN lv_string WITH 'SubSupplyType'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SUBSUPPLYTYPEDESC'        IN lv_string WITH 'SubSupplyTypeDesc'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SubSupplyTypeDESC'        IN lv_string WITH 'SubSupplyTypeDesc'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSACTIONTYPE'          IN lv_string WITH 'TransactionType'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'BUYERDTLS'                IN lv_string WITH 'BuyerDtls'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'GSTIN'                    IN lv_string WITH 'Gstin'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LGLNM'                    IN lv_string WITH 'LglNm'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRDNM'                    IN lv_string WITH 'TrdNm'  RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR1'                    IN lv_string WITH 'Addr1' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR2'                    IN lv_string WITH 'Addr2' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LOC'                      IN lv_string WITH 'Loc' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'PIN'                      IN lv_string WITH 'Pin' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'STCD'                     IN lv_string WITH 'Stcd' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SELLERDTLS'               IN lv_string WITH 'SellerDtls' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'GSTIN'                    IN lv_string WITH 'Gstin' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LGLNM'                    IN lv_string WITH 'LglNm' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRDNM'                    IN lv_string WITH 'TrdNm' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR1'                    IN lv_string WITH 'Addr1' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR2'                    IN lv_string WITH 'Addr2' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LOC'                      IN lv_string WITH 'Loc' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'PIN'                      IN lv_string WITH 'Pin' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'STCD'                     IN lv_string WITH 'Stcd' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'EXPSHIPDTLS'              IN lv_string WITH 'ExpShipDtls' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LGLNM'                    IN lv_string WITH 'LglNm' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR1'                    IN lv_string WITH 'Addr1' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LOC'                      IN lv_string WITH 'Loc' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'PIN'                      IN lv_string WITH 'Pin' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'STCD'                     IN lv_string WITH 'Stcd' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'DISPDTLS'                 IN lv_string WITH 'DispDtls' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '"NM'                      IN lv_string WITH '"Nm' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR1'                    IN lv_string WITH 'Addr1' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ADDR2'                    IN lv_string WITH 'Addr2' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'LOC'                      IN lv_string WITH 'Loc' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'PIN'                      IN lv_string WITH 'Pin' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'STCD'                     IN lv_string WITH 'Stcd' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ITEMLIST'                 IN lv_string WITH 'ItemList' RESPECTING CASE.


    REPLACE ALL OCCURRENCES OF 'PRODNAME'                IN lv_string WITH 'ProdName' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'PRODDESC'                IN lv_string WITH 'ProdDesc' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'HSNCD'                   IN lv_string WITH 'HsnCd' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'QTY'                     IN lv_string WITH 'Qty' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'UNIT'                    IN lv_string WITH 'Unit' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'ASSAMT'                  IN lv_string WITH 'AssAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'CGSTRT'                  IN lv_string WITH 'CgstRt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'CGSTAMT'                 IN lv_string WITH 'CgstAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SGSTRT'                  IN lv_string WITH 'SgstRt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'SGSTAMT'                 IN lv_string WITH 'SgstAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'IGSTRT'                  IN lv_string WITH 'IgstRt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'IGSTAMT'                 IN lv_string WITH 'IgstAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'CESRT'                   IN lv_string WITH 'CesRt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'CESAMT'                  IN lv_string WITH 'CesAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'OTHCHRG'                 IN lv_string WITH 'OthChrg' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'CESNONADVAMT'            IN lv_string WITH 'CesNonAdvAmt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALINVOICEAMOUNT'      IN lv_string WITH 'TotalInvoiceAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALCGSTAMOUNT'         IN lv_string WITH 'TotalCgstAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALSGSTAMOUNT'         IN lv_string WITH 'TotalSgstAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALIGSTAMOUNT'         IN lv_string WITH 'TotalIgstAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALCESSAMOUNT'         IN lv_string WITH 'TotalCessAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALCESSNONADVOLAMOUNT' IN lv_string WITH 'TotalCessNonAdvolAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TOTALASSESSABLEAMOUNT'   IN lv_string WITH 'TotalAssessableAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'OTHERAMOUNT'             IN lv_string WITH 'OtherAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'OTHERTCSAMOUNT'          IN lv_string WITH 'OtherTcsAmount' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSID'                 IN lv_string WITH 'TransId' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSNAME'               IN lv_string WITH 'TransName' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSMODE'               IN lv_string WITH 'TransMode' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'DISTANCE'                IN lv_string WITH 'Distance' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCNO'              IN lv_string WITH 'TransDocNo' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCDT'              IN lv_string WITH 'TransDocDt' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'VEHNO'                   IN lv_string WITH 'VehNo' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF 'VEHTYPE'                 IN lv_string WITH 'VehType' RESPECTING CASE.



    REPLACE ALL OCCURRENCES OF '**' IN lv_string WITH '*' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '"Distance": 0.0,' IN lv_string WITH '"Distance": 0,' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF ':0.0' IN lv_string WITH ':0' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '""' IN lv_string WITH 'null' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '111111' IN lv_string WITH 'null' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '*' IN lv_string WITH '' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '29AEEPh1574G1ZJ' IN lv_string WITH '29AEEPH1574G1ZJ' RESPECTING CASE.

    result = lv_string.
    ENDMETHOD.
ENDCLASS.
