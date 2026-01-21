CLASS yeway_bill_without_irn DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

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
             slno         TYPE string,
           "  prodname     TYPE string,
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
      IMPORTING invoice         TYPE char10
                VALUE(companycode)   TYPE CHAR4
                distance        TYPE string OPTIONAL
                transpoter_name TYPE string OPTIONAL
                transportid     TYPE string OPTIONAL
                transportdoc    TYPE string OPTIONAL
                vehiclenumber   TYPE string OPTIONAL
                Eway_generate   TYPE string OPTIONAL
      RETURNING VALUE(status)   TYPE string.


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
  PROTECTED             SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS YEWAY_BILL_WITHOUT_IRN IMPLEMENTATION.


  METHOD generated_eway_bill .

     DATA: BEGIN OF w_ewaybill,
            ackdt        TYPE string,
            ackno        TYPE string,
            ewbno        TYPE string,
            ewbdt        TYPE string,
            status2      TYPE string,
            success      TYPE string,
            ewbvalidtill TYPE string,
            km(30)       TYPE c,
          END OF w_ewaybill.

    FIELD-SYMBOLS:
      <datax>              TYPE data,
      <datay>              TYPE data,
      <dataz>              TYPE data,
      <data2>              TYPE data,
      <data3>              TYPE any,
      <field>              TYPE any,
      <field_ackdt>        TYPE any,
      <field_ackno>        TYPE any,
      <field_irn>          TYPE any,
      <field_ewbno>        TYPE any,
      <field_ewbdt>        TYPE any,
      <field_status>       TYPE any,
      <field_success>      TYPE any,
      <field_ewbvalidtill> TYPE any.

    DATA:error_msg_part1 TYPE string,
         error_msg_part2 TYPE string.

    DATA:vbeln(10) TYPE c.
    vbeln = invoice.
    vbeln = |{ vbeln ALPHA = IN }|.


**************************************************************************
    SELECT SINGLE *  FROM i_billingdocumentbasic  WHERE billingdocument = @vbeln INTO   @DATA(bill_head).
    SELECT siNGLE *  FROM i_billingdocumentpartner  WHERE billingdocument = @vbeln AND PartnerFunction = 'ZD' INTO @DATA(i_billingpart).

*************Billing partener "BUYER'

SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'RE'  AND c~billingdocument =  @vbeln
        "where C~BillingDocument = @variable
        INTO @DATA(buyeradd)  .

*    SELECT SINGLE * FROM   i_billingdocumentpartner AS a  INNER JOIN i_customer AS
*                b ON   ( a~customer = b~customer  )
*                LEFT JOIN  i_address_2 WITH PRIVILEGED ACCESS AS c ON   ( c~addressid = a~addressid  )
*                LEFT JOIN i_addrcurdfltmobilephonenumber AS d ON ( d~addressid = a~addressid  )
*                WHERE a~billingdocument = @vbeln
*                 AND a~partnerfunction = 'RE' INTO  @DATA(buyeradd)   .

*************Shipping Partner Details

         SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'WE'  AND c~billingdocument = @vbeln
        "where C~BillingDocument = @variable
        INTO @DATA(shippingadd)  .

*    SELECT SINGLE * FROM i_billingdocumentpartner AS a  INNER JOIN i_customer AS
*                b ON   ( a~customer = b~customer  )
*                LEFT JOIN  i_address_2 WITH PRIVILEGED ACCESS AS c ON   ( c~addressid = a~addressid  )
*                LEFT JOIN i_addrcurdfltmobilephonenumber AS d ON ( d~addressid = a~addressid  )
*                WHERE a~billingdocument = @vbeln
*                 AND a~partnerfunction = 'WE' INTO   @DATA(shippingadd)   .

**************Transporter Details
    SELECT SINGLE * FROM  I_BillingDocumentPartner WHERE BillingDocument = @vbeln
                          AND PartnerFunction =  'ZT' INTO @DATA(TRANSPORTPARTRNER) .
    SELECT SINGLE * FROM I_SUPPLIER WHERE Supplier = @TRANSPORTPARTRNER-SUPPLIER  INTO @DATA(TRANSPORTPARTRNER1)   .

*********************************
    SELECT SINGLE * FROM zsd_plant_address AS a INNER JOIN i_billingdocumentitembasic AS b ON ( a~plant =  b~plant )
    WHERE billingdocument =  @vbeln INTO @DATA(plantaddress) .

*************ITEM DETAILS
    SELECT * FROM i_billingdocumentitem   WHERE
    billingdocument  =  @vbeln AND billingquantity NE ''  INTO TABLE @DATA(billing_item)  .

*************Pricing data
    SELECT a~conditionamount,a~conditiontype , a~BillingDocument,a~BillingDocumentItem,a~conditionrateratio
    FROM i_billingdocumentitemprcgelmnt AS a  INNER JOIN   i_billingdocumentitem AS b
    ON ( a~billingdocument = b~billingdocument and a~BillingDocumentItem = b~BillingDocumentItem )  WHERE
    a~billingdocument  = @vbeln AND billingquantity NE '' INTO TABLE @DATA(b_item)   .

*    SELECT * FROM i_billingdocumentitemprcgelmnt AS a  INNER JOIN   i_billingdocumentitem AS b
*    ON ( a~billingdocument = b~billingdocument  )  WHERE
*    a~billingdocument  = @vbeln AND billingquantity NE '' INTO TABLE @DATA(b_item)   .
data header TYPE string.
DATA: lt_tax TYPE STANDARD TABLE OF I_BillingDocumentItem-TaxCode,
      lv_all_a0 TYPE abap_bool.
READ TABLE billing_item INTO DATA(HDWA) INDEX 1.

    wa_final-documentnumber = HDWA-BillingDocument.
    wa_final-supplytype        = 'Outward'.
    wa_final-subsupplytypedesc = 'Other'  .

IF ( bill_head-sddocumentcategory EQ 'M' and  bill_head-billingdocumenttype = 'F2' ) or ( bill_head-sddocumentcategory EQ 'M' and  bill_head-billingdocumenttype = 'JSTO' )  .

IF HDWA-BillingDocumentType = 'F2' OR HDWA-BillingDocumentType = 'JSTO'.
 SELECT TaxCode FROM I_BillingDocumentItem WITH PRIVILEGED ACCESS
 WHERE BillingDocument = @HDWA-BillingDocument GROUP BY TaxCode INTO TABLE @lt_tax.

  lv_all_a0 = abap_true.

  LOOP AT lt_tax INTO DATA(ls_tax).
    IF ls_tax <> 'A0'.
      lv_all_a0 = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_all_a0 = abap_true.
    wa_final-documenttype = 'BOS'.
  ELSE.
    wa_final-documenttype = 'INV'.
  ENDIF.

ELSE.

 wa_final-documenttype = 'INV'.

ENDIF.

    wa_final-subsupplytype     = '1'.
    wa_final-subsupplytypedesc = 'Supply' .

   elseIF bill_head-sddocumentcategory EQ 'M' and  bill_head-billingdocumenttype = 'F8'  .

    wa_final-documenttype = 'CHL'.
    wa_final-subsupplytype     = '8'.
    wa_final-subsupplytypedesc = 'Others' .

    ELSEIF bill_head-sddocumentcategory = 'O'.
      IF bill_head-billingdocumenttype = 'CBRE'.
        wa_final-supplytype     = 'INWARD'.       "other
        wa_final-documenttype = 'OTH'.
        wa_final-subsupplytype     = '7'.       "other
      ELSE.
        wa_final-subsupplytype     = '8'.       "other
        wa_final-documenttype = 'CRN'.
      ENDIF.
    ELSEIF bill_head-sddocumentcategory = 'P'.
      wa_final-subsupplytype     = '8'.       "other
      wa_final-documenttype = 'DBN'.
    ELSEIF bill_head-sddocumentcategory = 'U'.
      wa_final-subsupplytype     = '8'.       "other
      wa_final-documenttype = 'CHL'.
    ENDIF.


    wa_final-documentdate = |{ bill_head-billingdocumentdate+6(2) }/{ bill_head-billingdocumentdate+4(2) }/{ bill_head-billingdocumentdate+0(4) }|.


    READ TABLE billing_item INTO DATA(wa_bill) INDEX 1.

    SELECT SINGLE a~addressid,
                  a~PlantName,
                  b~addresssearchterm2 AS gstin,
                  b~careofname AS lglnm,
                  b~AddresseeFullName as Name,
                  b~streetname AS addr1,
                  b~cityname AS addr2,
                  b~districtname AS city,
                  b~postalcode AS pin,
                  b~region AS stcd
    FROM i_plant AS a
    LEFT JOIN i_address_2 WITH PRIVILEGED ACCESS AS b ON ( a~addressid = b~addressid )
    WHERE plant = @wa_bill-plant INTO @DATA(sellerplantaddress).

SELECT SINGLE * FROM  zship_add WHERE Plant = @plantaddress-a-Plant INTO @DATA(plant_add)  .

       SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN I_Supplier AS b ON ( a~ReferenceBusinessPartner = b~Supplier )
        INNER JOIN ZI_Address_2 WITH PRIVILEGED ACCESS AS c ON ( c~AddressID = B~AddressID )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'ZD'  AND A~billingdocument = @vbeln
        INTO @DATA(disp)  .

   if SY-SYSID = 'ALU'  or SY-SYSID = 'AWL' .
   wa_final-sellerdtls-gstin    =  '08AAFCD5862R018' .
   ELSE.
   SELECT SINGLE * FROM zsales_tmg WHERE plant = @wa_bill-Plant INTO @data(gst).
   wa_final-sellerdtls-gstin    =  gst-gstno.
   ENDIF.


    IF sellerplantaddress-stcd = 'RJ'.
       sellerplantaddress-stcd = '08'.
    ENDIF.

    wa_final-sellerdtls-lglnm    =  plant_add-AddresseeFullName.
    wa_final-sellerdtls-trdnm    =  plant_add-AddresseeFullName.
    wa_final-sellerdtls-addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } |.
    wa_final-sellerdtls-addr2    =  | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | .
    wa_final-sellerdtls-loc      =  plant_add-CityName.
    wa_final-sellerdtls-stcd     =  plant_add-Region .
    wa_final-sellerdtls-pin      =  plant_add-PostalCode.


    REPLACE ALL OCCURRENCES OF ',' IN wa_final-sellerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-sellerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-sellerdtls-addr2 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-sellerdtls-addr2 WITH ' ' .

 IF i_BillingPart-PartnerFunction = 'ZD' and disp-a-ReferenceBusinessPartner <> ''.

    wa_final-dispdtls-nm       =  disp-C-AddresseeFullName.
    wa_final-dispdtls-addr1    =  | { disp-c-StreetName } { disp-c-StreetPrefixName1 }  { disp-c-StreetPrefixName2 } |.
    wa_final-dispdtls-addr2    =  | { disp-c-StreetSuffixName1 }  { disp-c-StreetSuffixName2 } { disp-c-DistrictName }  | .
    wa_final-dispdtls-loc      =  disp-c-CityName.
    wa_final-dispdtls-stcd     =  disp-c-Region.
    wa_final-dispdtls-pin      =  disp-c-PostalCode.

  else.
    wa_final-dispdtls-nm       =  plant_add-AddresseeFullName.
    wa_final-dispdtls-addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } |.
    wa_final-dispdtls-addr2    =  | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | .
    wa_final-dispdtls-loc      =  plant_add-CityName.
    wa_final-dispdtls-stcd     =  plant_add-Region .
    wa_final-dispdtls-pin      =   plant_add-PostalCode.
  endIF.


    REPLACE ALL OCCURRENCES OF ',' IN wa_final-dispdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-dispdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-dispdtls-addr2 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-dispdtls-addr2 WITH ' ' .

*    IF buyeradd-b-country EQ 'IN' .
IF bill_head-CustomerGroup = '03' .
    wa_final-buyerdtls-gstin = 'URP'.
ELSE.
    wa_final-buyerdtls-gstin = buyeradd-b-taxnumber3.
ENDIF.
    wa_final-buyerdtls-lglnm = buyeradd-b-CustomerName."buyeradd-b-customername.
    wa_final-buyerdtls-trdnm = buyeradd-b-CustomerName ." buyeradd-b-customername.
    wa_final-buyerdtls-addr1 = |{ buyeradd-C-Street } { buyeradd-C-StreetName } { buyeradd-C-StreetPrefixName1 } { buyeradd-C-StreetPrefixName2 } { buyeradd-C-StreetSuffixName1 } { buyeradd-C-StreetSuffixName2 }|.
    wa_final-buyerdtls-addr2 = |{ buyeradd-b-DistrictName } { buyeradd-k-RegionName } { buyeradd-m-CountryName } |.
    wa_final-buyerdtls-loc   = buyeradd-c-cityname .
    wa_final-buyerdtls-pin   = buyeradd-c-postalcode  .
    wa_final-buyerdtls-stcd  = buyeradd-c-region  .

    REPLACE ALL OCCURRENCES OF ',' IN wa_final-buyerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-buyerdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-buyerdtls-addr2 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-buyerdtls-addr2 WITH ' ' .



    wa_final-expshipdtls-lglnm    =  Shippingadd-b-CustomerName .
    wa_final-expshipdtls-addr1    = |{ Shippingadd-C-Street } { Shippingadd-C-StreetName } { Shippingadd-C-StreetPrefixName1 } { Shippingadd-C-StreetPrefixName2 } { Shippingadd-C-StreetSuffixName1 } { Shippingadd-C-StreetSuffixName2 }|.
    wa_final-expshipdtls-loc      = shippingadd-c-cityname.
    wa_final-expshipdtls-stcd     = shippingadd-c-region.
    wa_final-expshipdtls-pin      = shippingadd-c-postalcode.
    REPLACE ALL OCCURRENCES OF ',' IN wa_final-expshipdtls-addr1 WITH ' ' .
    REPLACE ALL OCCURRENCES OF '’' IN wa_final-expshipdtls-addr1 WITH ' ' .


IF ( wa_final-dispdtls-pin      =  wa_final-sellerdtls-pin ) AND ( wa_final-buyerdtls-pin  = wa_final-expshipdtls-pin ) .
   wa_final-transactiontype     = '1'.
ELSEIF ( wa_final-dispdtls-pin  =  wa_final-sellerdtls-pin ) AND ( wa_final-buyerdtls-pin  <> wa_final-expshipdtls-pin ) .
   wa_final-transactiontype     = '2'.
ELSEIF ( wa_final-dispdtls-pin  <>  wa_final-sellerdtls-pin ) AND ( wa_final-buyerdtls-pin  = wa_final-expshipdtls-pin ) .
   wa_final-transactiontype     = '3'.
ELSEIF ( wa_final-dispdtls-pin  <>  wa_final-sellerdtls-pin ) AND ( wa_final-buyerdtls-pin  <> wa_final-expshipdtls-pin ) .
   wa_final-transactiontype     = '4'.
ELSE .
   wa_final-transactiontype     = '1'.
ENDIF.

    LOOP AT  billing_item INTO DATA(bill_item) .
    DATA N TYPE STRING.
    N = N + 1.
      DATA:unitprice TYPE yinv_stu-igstamt,
           totamt    TYPE yinv_stu-igstamt.

      SELECT SINGLE conditionbaseamount FROM i_billingdocumentitemprcgelmnt WHERE billingdocument = @bill_item-billingdocument
                                          AND billingdocumentitem = @bill_item-BillingDocumentItem
                                          AND conditionbaseamount IS NOT INITIAL AND  conditiontype IN ( 'JOIG' ,'JOCG' ,'JOSG' ) INTO @DATA(baseamount)  .

      READ TABLE b_item INTO DATA(zbasic) WITH KEY ConditionType  = 'ZR00'
                                                   BillingDocumentItem = bill_ITEM-BillingDocumentItem.
IF zbasic-ConditionAmount = 0.
      READ TABLE b_item INTO zbasic WITH KEY ConditionType        = 'ZR01'
                                             BillingDocumentItem = bill_ITEM-BillingDocumentItem.
ENDIF.
IF zbasic-ConditionAmount = 0.
      READ TABLE b_item INTO zbasic WITH KEY ConditionType        = 'PCIP'
                                             BillingDocumentItem = bill_ITEM-BillingDocumentItem.
ENDIF.
      IF baseamount IS NOT INITIAL.
      totamt = baseamount.
      ELSE.
      totamt = ( zbasic-ConditionAmount ) * bIll_head-AccountingExchangeRate.
      ENDIF.

    SELECT SINGLE * FROM ZMAT_TEXT WHERE Product = @bill_item-Material and Plant = @bill_ITEM-Plant INTO @DATA(MATDES).

      wa_itemlist-slno        =  N.
   "   wa_itemlist-prodname    =  bill_item-Material .

   IF MATDES-mattext <> ''.
    wa_itemlist-proddesc    = MATDES-mattext .
   ELSE.
    wa_itemlist-proddesc    = bill_item-billingdocumentitemtext .
   ENDIF  .


      SELECT SINGLE * FROM i_productplantbasic AS a LEFT JOIN i_product AS b ON ( a~product = b~product )
       WHERE a~product = @bill_item-material AND a~plant = @bill_item-plant  INTO @DATA(hsnsac).

        IF bill_head-DistributionChannel = '06' .
        wa_itemlist-hsncd   = '551599'.
      ELSEIF hsnsac-b-producttype = 'SERV'.
        wa_itemlist-hsncd      =  hsnsac-a-consumptiontaxctrlcode .
      ELSE.
        wa_itemlist-hsncd      =  hsnsac-a-consumptiontaxctrlcode .
      ENDIF.
      wa_itemlist-qty        =  bill_item-billingquantity .
      IF bill_item-billingquantityunit           = 'TO' .
        wa_itemlist-unit       =  'TON' .
      ELSEIF bill_item-billingquantityunit      = 'MTR' or bill_item-billingquantityunit      = 'M'.
        wa_itemlist-unit       =  'MTR' .
      ELSEIF bill_item-billingquantityunit     = 'KG' .
        wa_itemlist-unit       =  'KGS' .
        ELSEIF bill_item-billingquantityunit     = 'PC' OR  bill_item-billingquantityunit     = 'ST' .
        wa_itemlist-unit       =  'PCS' .
      ELSEIF bill_item-billingquantityunit+0(3) = 'BAG' .
        wa_itemlist-unit       =  'BAGS'.
      ELSEIF bill_item-billingquantityunit+0(3) = 'NO' .
        wa_itemlist-unit       =  'NOS'.
     ELSEIF bill_item-billingquantityunit+0(3) = 'PAK' .
        wa_itemlist-unit       =  'PAC'.
     ELSEIF bill_item-billingquantityunit+0(3) = 'CS' .
        wa_itemlist-unit       =  'CTN'.
      ELSE .
        wa_itemlist-unit        =  'OTH'.
      ENDIF .




      """""""""""""""""""""BASIC PRICE""""""""""""""""""""""""""
*      SELECT  from @b_item as a FIELDS sum( a~conditionamount ) WHERE  conditiontype = 'PCIP' and
*       BillingDocument = @bill_item-BillingDocument into @data(zbasic).
**      READ TABLE b_item INTO DATA(zbasic) WITH KEY conditiontype = 'PCIP'
**       BillingDocument = bill_item-BillingDocument .
*      IF sy-subrc = 0.
*      IF totamt = 0.
*        totamt     = zbasic.
*      ENDIF.
*      ENDIF .
*
* "      AND ( ( ConditionType = 'ZR09' AND d~ConditionAmount <> 0 ) OR ConditionType = 'ZR10' )   and )
*       SELECT  from @b_item as a FIELDS sum( a~conditionamount ) WHERE  conditiontype = 'ZR00' and
*       BillingDocument = @bill_item-BillingDocument AND
*       BillingDocumentItem = @bill_item-BillingDocumentItem into @zbasic.
**      READ TABLE b_item INTO zbasic WITH KEY a-conditiontype = 'ZR09'
**       a-BillingDocument = bill_item-BillingDocument .
*      IF sy-subrc = 0.
*      IF totamt = 0.
*        totamt     = zbasic.
*      ENDIF.
*      ENDIF .
*
*       SELECT  from @b_item as a FIELDS sum( a~conditionamount ) WHERE  conditiontype = 'ZR01' and
*       BillingDocument = @bill_item-BillingDocument AND
*       BillingDocumentItem = @bill_item-BillingDocumentItem into @zbasic.
**       READ TABLE b_item INTO zbasic WITH KEY a-conditiontype = 'ZR10'
**       a-BillingDocument = bill_item-BillingDocument.
*       IF sy-subrc = 0.
*       IF totamt = 0.
*        totamt     = zbasic.
*      ENDIF.
*      ENDIF .


      """"""""""""""""""""""""""""""""""""""discount"""""""""""""""""""""""""""""""""""""
*      SELECT SUM( conditionamount )    FROM i_billingdocumentitemprcgelmnt
*       WHERE   conditiontype IN ( 'ZDPQ','ZDPR', 'ZDFA' )
*       AND billingdocument = @bill_item-billingdocument AND billingdocumentitem = @bill_item-billingdocumentitem
*        INTO @DATA(discount) .


      wa_itemlist-assamt       = totamt .
      wa_itemlist-cesnonadvamt = '0'.

      """""""""""""""""""""""""""""""""""""""""""""""TAX DATA*******************************
      READ TABLE b_item INTO DATA(zbasicigst) WITH KEY conditiontype = 'JOIG'  BillingDocument = bill_item-BillingDocument BillingDocumentItem = BILL_ITEM-BillingDocumentItem.
      IF sy-subrc EQ 0 .

        wa_itemlist-igstrt      =  zbasicigst-conditionrateratio .
        wa_itemlist-igstamt    =  zbasicigst-conditionamount ."  * bill_head-accountingexchangerate   .

      ELSE.

        READ TABLE b_item INTO DATA(zbasicgst) WITH KEY conditiontype = 'JOCG'  billingdocument = bill_item-billingdocument BillingDocumentItem = BILL_ITEM-BillingDocumentItem .
        wa_itemlist-cgstrt      =  zbasicgst-conditionrateratio." * 2.
        wa_itemlist-cgstamt    =  zbasicgst-conditionamount ." * bill_head-accountingexchangerate   .

        READ TABLE b_item INTO DATA(zbasisgst) WITH KEY conditiontype = 'JOSG'   billingdocument = bill_item-billingdocument BillingDocumentItem = BILL_ITEM-BillingDocumentItem.
        wa_itemlist-sgstrt     =  zbasisgst-conditionrateratio .
        wa_itemlist-sgstamt    =  zbasisgst-conditionamount ."* bill_head-accountingexchangerate .

      ENDIF .
*
       SELECT SINGLE from @b_item as a FIELDS sum( a~conditionamount ) as conditionamount  WHERE  conditiontype = 'JTC1' and
       BillingDocument = @bill_item-BillingDocument AND BillingDocumentItem = @BILL_ITEM-BillingDocumentItem into @data(tcs).

      "READ TABLE b_item INTO DATA(tcs) WITH KEY a-conditiontype = 'JTC1'  a-BillingDocument = bill_item-BillingDocument .
*      IF sy-subrc EQ 0 .
*        DATA(tcsamt) = bill_item-ztcs_amt .
*      ENDIF.

             SELECT SINGLE from @b_item as a FIELDS sum( a~conditionamount ) as conditionamount  WHERE  conditiontype = 'ZDOF' and
       BillingDocument = @bill_item-BillingDocument into @data(zbasicroundoff).
      IF sy-subrc EQ 0 .
        wa_final-otheramount    =  zbasicroundoff    .
      ENDIF.
      wa_itemlist-othchrg              = '0'."otchg  * bill_head-accountingexchangerate .
      wa_final-totalassessableamount   = wa_final-totalassessableamount  + wa_itemlist-assamt  .
      wa_final-totaligstamount         = wa_final-totaligstamount + wa_itemlist-igstamt .
      wa_final-totalsgstamount         = wa_final-totalsgstamount + wa_itemlist-sgstamt .
      wa_final-totalcgstamount         = wa_final-totalcgstamount + wa_itemlist-cgstamt .
   "   wa_final-otheramount             = '0'."wa_final-otheramount + wa_itemlist-othchrg.
      wa_final-othertcsamount          = wa_final-othertcsamount  .
      wa_final-totalinvoiceamount      = wa_final-totalinvoiceamount + wa_itemlist-assamt +
                                         wa_itemlist-igstamt + wa_itemlist-cgstamt +
                                         wa_itemlist-sgstamt + wa_itemlist-othchrg .



      IF wa_itemlist-cgstrt IS INITIAL.
        wa_itemlist-cgstrt = '0'.
      ENDIF.
      IF wa_itemlist-cgstamt IS INITIAL.
        wa_itemlist-cgstamt = '0'.
      ENDIF.
      IF wa_itemlist-sgstrt IS INITIAL.
        wa_itemlist-sgstrt = '0'.
      ENDIF.
      IF wa_itemlist-sgstamt IS INITIAL.
        wa_itemlist-sgstamt = '0'.
      ENDIF.
      IF wa_itemlist-igstrt IS INITIAL.
        wa_itemlist-igstrt = '0'.
      ENDIF.
      IF wa_itemlist-igstamt IS INITIAL.
        wa_itemlist-igstamt = '0'.
      ENDIF.
      IF wa_itemlist-cesrt IS INITIAL.
        wa_itemlist-cesrt = '0'.
      ENDIF.
      IF wa_itemlist-cesamt IS INITIAL.
        wa_itemlist-cesamt = '0'.
      ENDIF.
      IF wa_itemlist-othchrg IS INITIAL.
        wa_itemlist-othchrg = '0'.
      ENDIF.
      IF wa_itemlist-cesnonadvamt IS INITIAL.
        wa_itemlist-cesnonadvamt = '0'.
      ENDIF.

      APPEND wa_itemlist TO itemlist .
      CLEAR :  wa_itemlist,unitprice ,totamt,zbasic,baseamount.

    ENDLOOP .

    wa_final-totalinvoiceamount =  wa_final-totalinvoiceamount + + zbasicroundoff .

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
    wa_final-distance   = distance        .
    wa_final-transname  = transpoter_name .
    wa_final-transid    = transportid     .
    wa_final-transdocno = bIll_head-yy1_lrno1_bdh   .

    if  bIll_head-YY1_LRDate_BDH <> '00000000'.
    wa_final-transdocdt =   |{ bIll_head-YY1_LRDate_BDH+6(2) }/{ bIll_head-YY1_LRDate_BDH+4(2) }/{ bIll_head-YY1_LRDate_BDH+0(4) }| .
    endiF.

    IF wa_final-transname IS INITIAL.
      SELECT SINGLE * FROM   i_billingdocumentpartner AS a  INNER JOIN i_supplier AS
                  b ON   ( b~supplier = a~supplier  )
                  LEFT JOIN  i_address_2 WITH PRIVILEGED ACCESS AS c ON   ( c~addressid = a~addressid  )
*                  LEFT JOIN i_addrcurdfltmobilephonenumber AS d ON ( d~addressid = a~addressid  )
                  WHERE a~billingdocument = @vbeln
                   AND a~partnerfunction = 'SP' INTO  @DATA(transporter)   .
      wa_final-transname  = transporter-b-suppliername.
      wa_final-transid    = transporter-b-taxnumber3     .
    ENDIF.
        SELECT SINGLE * FROM I_BillingDocumentItem WHERE billingdocument = @vbeln INTO @DATA(portdata2)  .
        SELECT SINGLE * FROM I_DeliveryDocument WHERE DeliveryDocument = @portdata2-ReferenceSDDocument INTO @DATA(SHIP).
       IF bill_head-YY1_VehicleNo_BDH  IS INITIAL.
        wa_final-vehno       = vehiclenumber.
        ELSE.
         wa_final-vehno         =   bill_head-YY1_VehicleNo_BDH ."vehiclenumber  .
        ENDIF.
    wa_final-vehtype    = 'R'   .
      if SHIP-ShippingType <> ''.
      SHIFT  SHIP-ShippingType LEFT DELETING LEADING '0' .
      else.
      SHIP-ShippingType = '1'.
      ENDIF.
    wa_final-transmode  = SHIP-ShippingType  . "|{ bill_head-YY1_TransportMode_BDH ALPHA = OUT }|.

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


    REPLACE ALL OCCURRENCES OF 'SLNO'                    IN lv_string WITH 'SlNo'.
   " REPLACE ALL OCCURRENCES OF 'PRODNAME'                IN lv_string WITH 'ProdName' RESPECTING CASE.
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

    REPLACE ALL OCCURRENCES OF '""' IN lv_string WITH 'null' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '111111' IN lv_string WITH 'null' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '*' IN lv_string WITH '' RESPECTING CASE.
    REPLACE ALL OCCURRENCES OF '10ABIPh3151N1ZW' IN lv_string WITH '10ABIPH3151N1ZW'.
    REPLACE ALL OCCURRENCES OF '29AEEPh1574G1ZJ' IN lv_string WITH '29AEEPH1574G1ZJ' RESPECTING CASE.


     CLEAR   access_token .
    DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invoice
                                                                                  companycode = CompanyCode
                                                                                  lv_string = lv_string
                                                                                  eway_generate = eway_generate
                                                                                  lv_all_a0 = lv_all_a0 )  .


    DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).
    IF lr_d1 IS BOUND.
      ASSIGN lr_d1->* TO <datax>.
      IF sy-subrc = 0 .
        ASSIGN COMPONENT 'error_message' OF STRUCTURE <datax> TO FIELD-SYMBOL(<error_msg1>).
        IF <error_msg1> IS ASSIGNED.
          error_msg_part1 = <error_msg1>->*.
        ELSE.
          ASSIGN COMPONENT `GOVT_RESPONSE` OF STRUCTURE <datax> TO <field>    .
        ENDIF.

        IF <field> IS ASSIGNED.
          ASSIGN <field>->* TO <data2>  .
          IF sy-subrc = 0 .
            ASSIGN COMPONENT `EwbNo` OF STRUCTURE <data2>  TO   <field_ewbno>     .
            IF sy-subrc = 0.
              ASSIGN  <field_ewbno>->* TO  <field_ewbno>  .
              w_ewaybill-ewbno  = <field_ewbno> .
            ENDIF.

            ASSIGN COMPONENT `EwbDt` OF STRUCTURE <data2>  TO   <field_ewbdt>     .
            IF sy-subrc = 0.
              ASSIGN  <field_ewbdt>->* TO  <field_ewbdt>  .
              w_ewaybill-ewbdt  = <field_ewbdt> .
            ENDIF.

            ASSIGN COMPONENT `status` OF STRUCTURE <data2>  TO   <field_status>     .
            IF sy-subrc = 0.
              ASSIGN  <field_status>->* TO  <field_status>  .
              w_ewaybill-status2  = <field_status> .
            ENDIF.

            ASSIGN COMPONENT `Success` OF STRUCTURE <data2>  TO   <field_success>     .
            IF sy-subrc = 0.
              ASSIGN  <field_success>->* TO  <field_success>  .
              w_ewaybill-success  = <field_success> .
            ENDIF.


            ASSIGN COMPONENT `ewbvalidtill` OF STRUCTURE <data2>  TO FIELD-SYMBOL(<field_validupto>)   .
            IF sy-subrc = 0.
              ASSIGN <field_validupto>->* TO <field_validupto> .
              i_eway-validupto = <field_validupto> .
            ENDIF.

            ASSIGN COMPONENT `alert` OF STRUCTURE <data2>  TO  FIELD-SYMBOL(<km>)     .
            IF sy-subrc = 0.
              IF <km> IS BOUND.
                w_ewaybill-km = reverse( <km>->* ).
                SPLIT w_ewaybill-km+1(*) AT '' INTO w_ewaybill-km DATA(other).
                w_ewaybill-km = w_ewaybill-km+1(*).
                w_ewaybill-km = reverse( w_ewaybill-km ).
              ENDIF.
            ENDIF.
          ENDIF.

        ENDIF .
      ENDIF.

      """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      DATA respo  TYPE yewayrescoll .
      DATA ewaybilldata TYPE yj1ig_ewaybill .


      IF w_ewaybill-ewbno IS NOT INITIAL.
        SELECT SINGLE sddocumentcategory, companycode FROM i_billingdocumentbasic
              WHERE billingdocument = @vbeln  INTO (@DATA(bill_typ),@DATA(comp) )   .

 DATA distance1 TYPE STRING.
      IF distance IS INITIAL .
      distance1 =  w_ewaybill-km.
      ELSE.
      distance1 = distance.
      ENDIF.

      EWAYBILLDATA-distance    =  distance1 .
      EWAYBILLDATA-transporter = transpoter_name .
      EWAYBILLDATA-transportid = transportid .

      DATA(USERID) = cl_abap_context_info=>get_user_technical_name( ) .
      EWAYBILLDATA-createdby = USERID.

      IF  i_eway-validupto  IS NOT INITIAL .
      ewaybilldata-vdtodate    =   i_eway-validupto+0(4) &&  i_eway-validupto+5(2) &&  i_eway-validupto+8(2).
      ENDIF.
        ewaybilldata-bukrs       =  comp.
        ewaybilldata-doctyp      =  wa_final-documenttype.
        ewaybilldata-docno       =  vbeln  .
        ewaybilldata-gjahr       =  sy-datum+0(4)  .
        ewaybilldata-ebillno     =  w_ewaybill-ewbno .
        ewaybilldata-egen_dat    =  sy-datum .
        ewaybilldata-status      =  'ACT'   .
        ewaybilldata-vehiclenum  =  vehiclenumber .

        MODIFY yj1ig_ewaybill FROM @ewaybilldata  .
        COMMIT WORK AND WAIT.


        DATA ewaypartb TYPE zeway_part_b .
*        IF i_eway-ewbnumber IS NOT INITIAL.
        ewaypartb-docno       = vbeln .
        ewaypartb-ewbnumber   = w_ewaybill-ewbno.
        ewaypartb-updateddate = i_eway-updateddate.
        ewaypartb-validupto   = i_eway-validupto.
        ewaypartb-validupto = |{ ewaypartb-validupto+8(2) }/{ ewaypartb-validupto+5(2) }/{ ewaypartb-validupto+0(4) }|.
        MODIFY zeway_part_b FROM @ewaypartb  .

      ELSE .

      ENDIF  .

    ENDIF.

    IF  w_ewaybill-ewbno  IS NOT INITIAL.
      status = |EWAY Bill No.-{ w_ewaybill-ewbno  } Generated .|.
    ELSEIF error_msg_part1 IS NOT INITIAL.
      status = |EWAY Bill -{ error_msg_part1 } |.
    ELSE.
      status = |EWAY Bill -{ result9 } |.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
