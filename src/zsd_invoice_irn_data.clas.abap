CLASS zsd_invoice_irn_data DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun .

    CLASS-DATA : distan TYPE string .
    "CLASS FOR IRN GENERATION
     CLASS-METHODS :

      get_table_fields
        IMPORTING VALUE(invoice)       TYPE CHAR10
                  VALUE(companycode)   TYPE CHAR4
                  VALUE(irngenrate)    TYPE string
                  VALUE(eway_generate) TYPE string
                  VALUE(distance)      TYPE string OPTIONAL
                  transpoter_name      TYPE string OPTIONAL
                  transportid          TYPE string OPTIONAL
                  transportdoc         TYPE string OPTIONAL
                  vehiclenumber        TYPE string OPTIONAL
        RETURNING VALUE(result)        TYPE string  ,

        dateformat
        IMPORTING date          TYPE char10
        RETURNING VALUE(status) TYPE string .
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZSD_INVOICE_IRN_DATA IMPLEMENTATION.


 METHOD dateformat.


    DATA gv TYPE string .
    DATA gv1 TYPE string .
    DATA gv2 TYPE string .
    DATA gv3 TYPE string .
    gv3 = date+0(4)  .
    gv2 = date+4(2)  .
    gv1 = date+6(2)  .
    CONCATENATE gv1 '/' gv2 '/' gv3 INTO gv .
    status = gv .
  ENDMETHOD.


 METHOD get_table_fields .
    DATA: vbeln TYPE c LENGTH 10.
    DISTAN = distance .

   DATA : ITEMLIST     TYPE TABLE OF YEI_ITEMLIST_TT ,
          WA_ITEMLIST1 TYPE YEI_ITEMLIST_TT .

   DATA : it_data2     TYPE STANDARD TABLE OF Yinv_stu,
           wa_data2    TYPE  Yinv_stu.

   DATA : it_json_data TYPE TABLE OF  Yqwerty,
          wa_json_data TYPE Yqwerty,
          trandtls     TYPE Yei_trandtls_tt,
          docdtls      TYPE Yei_docdtls_st,
          sellerdtls   TYPE Yei_sellerdtls_tt,
          buyerdtls    TYPE Yei_buyerdtls_tt,
          dispdtls     TYPE Yei_dispdtls_t1,
          shipdtls     TYPE Yei_shipdtls_tt,
          it_itemlist  TYPE TABLE OF YeiV_itemlist_tt,
          wa_itemlist  TYPE Yei_itemlist_tt,
          bchdtls      TYPE Yei_bchdtls_tt,
          attribdtls   TYPE Yei_attribdtls_tt,
          valdtls      TYPE Yei_valdtls_tt,
          paydtls      TYPE Yei_paydtls_tt,
          refdtls      TYPE Yei_refdtls_tt,
          docpre       TYPE Yei_docperddt_st,
          precdocdtls  TYPE Yei_precdocdtls_tt,
          contrdtls    TYPE Yei_contrdtls_tt,
          addldocdtls  TYPE Yei_addldocdtls_tt,
          expdtls      TYPE Yei_expdtls_tt,
          ewbdtls      TYPE Yei_ewbdtls_st.

   DATA : it_json_data2 TYPE TABLE OF  yqwerty2,
          wa_json_data2 TYPE yqwerty2.


   DATA : it_json_data3 TYPE TABLE OF  yqwerty3,
          wa_json_data3 TYPE yqwerty3.


   DATA : it_json_data4 TYPE TABLE OF  yqwerty4,
          wa_json_data4 TYPE yqwerty4.




    DATA : IGSTAMT TYPE YINV_STU-igstamt.
    DATA : CGSTAMT TYPE YINV_STU-igstamt.
    DATA : SGSTAMT TYPE YINV_STU-igstamt.
    data : othamount  type     YINV_STU-igstamt .


  DATA: vbeln1 TYPE c LENGTH 10.
  vbeln1  =    |{ invoice ALPHA = in }|.

****************************************Ports*****************************
*  DATA(port) = 'MUNDRA' .

**************************************************************************
 SELECT single *  FROM i_billingdocumentbasic  where BillingDocument = @vbeln1 INTO   @DATA(bIll_head).
 SELECT  SINGLE *  FROM I_BillingDocumentPartner  where BillingDocument = @vbeln1 AND PartnerFunction = 'ZD' INTO  @DATA(i_BillingPart).

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""""""""""TIME CHANGE """"""""""""""""""""""""""""""""

DATA: lo_tstmp   TYPE TZNTSTMPS,
      INV_TIME  TYPE  sy-uzeit,
      INV_date  TYPE  sy-datum.
if bIll_head-BillingDocumentDate <> 00000000 .

   DATA GG TYPE STRING.
      convert date bIll_head-BillingDocumentDate  time bIll_head-CreationTime
      into time stamp lo_tstmp time zone 'UTC'.    " HHH

data(INV2)   = cl_abap_tstmp=>add( tstmp = lo_tstmp
                                secs  =  19800 ).



CONVERT TIME STAMP INV2 TIME ZONE 'UTC' INTO DATE inv_date TIME inv_time.

ENDIF.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

*************Billing partener "BUYER'
* select single * from   I_BillingDocumentPartner as a  inner JOIN i_customer as
*             b on   ( a~Customer = b~Customer  ) where a~BillingDocument = @vbeln1
*              and a~PartnerFunction = 'RE' into  @DATA(buyeradd)   .

SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'RE'  AND c~billingdocument =  @vbeln1
        "where C~BillingDocument = @variable
        INTO @DATA(buyeradd)  .

*************Shipping Partner Details
* Select single * from I_BillingDocumentPartner as a  inner JOIN i_customer as
*             b on   ( a~Customer = b~Customer  ) where a~BillingDocument = @vbeln1
*              and a~PartnerFunction = 'WE' into   @DATA(Shippingadd)   .

         SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'WE'  AND c~billingdocument = @vbeln1
        "where C~BillingDocument = @variable
        INTO @DATA(Shippingadd)  .


 SELECT single * FROM ZSD_PLANT_ADDRESS as a inner join I_BILLINGDOCUMENTITEMBASIC as b on ( a~Plant =  b~Plant )
 where BillingDocument =  @vbeln1 into @data(plantaddress) .

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
SELECT SINGLE * FROM  zship_add WHERE Plant = @plantaddress-a-Plant INTO @DATA(plant_add)  .

         SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN I_Supplier AS b ON ( a~ReferenceBusinessPartner = b~Supplier )
        INNER JOIN ZI_Address_2 WITH PRIVILEGED ACCESS AS c ON ( c~AddressID = B~AddressID )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'ZD'  AND A~billingdocument = @vbeln1
        INTO @DATA(disp)  .

*************ITEM DETAILS
 SELECT * FROM I_BillingDocumentItem   WHERE
 BillingDocument  =  @vbeln1 AND BillingQuantity NE ''  INTO TABLE @DATA(BILLING_ITEM)  .

*************Pricing data
 SELECT * FROM I_BillingDocumentItemPrcgElmnt AS A  INNER JOIN   I_BillingDocumentItem AS B
 ON ( A~BillingDocument = B~BillingDocument AND a~BillingDocumentItem = b~BillingDocumentItem )  WHERE
 A~BillingDocument  = @vbeln1 AND BillingQuantity NE '' INTO TABLE @DATA(B_ITEM)   .


    """"""""""""""""""""""""""""""""""""""""""""""""""Postal & State Code for Port Of Loading""""""""""""""""""""""""""""""""""""""""
            reaD TABLE BILLING_ITEM intO data(wa_BILLING) inDEX 1.
            SELECT SINGLE * FROM I_DeliveryDocument WHERE DeliveryDocument = @wa_BILLING-ReferenceSDDocument INTO @DATA(SHIP).

            SELECT SINGLE * FROM zpregen_exi WHERE docno = @vbeln1 AND Doctype = 'PO'  INTO @DATA(portdata1) .


        DATA: Postal    TYPE STRING,
              LOC       TYPE STRING,
              StateCode TYPE STRING.

      Postal    =   portdata1-POLPinCode .
      StateCode =   portdata1-POLStCode.
      LOC       =   portdata1-Portofloading.

""""""""""""""""""""""""""""""""""""""""""""""""""Postal & State Code for Port Of Loading""""""""""""""""""""""""""""""""""""""""


  IF bill_head-distributionchannel = '17' .

     IF buyeradd-b-country NE 'IN' AND buyeradd-b-taxnumber3 IS   INITIAL.  .
        wa_data2-taxsch     =  'GST'.
        wa_data2-version    =  '1.1'.
        wa_data2-irn        =  'GST'.

        IF bill_head-CustomerGroup = '01'.
        wa_data2-suptyp     = 'EXPWP'.
        ELSEIF bill_head-CustomerGroup = '02'.
        wa_data2-suptyp     = 'EXPWOP'.
        ENDIF.

        wa_data2-regrev     =  'N'.
        wa_data2-ecmgstin   =  ' '.
     "BUYER DETAILS
        wa_data2-b_pin      = '999999'.
        wa_data2-b_pos      = '96'.
        wa_data2-b_state    = '96'.

    ELSEIF  bill_head-distributionchannel = '17' AND buyeradd-b-country EQ 'IN'.
      wa_data2-taxsch     =  'GST'.
      wa_data2-version    =  '1.1'.
      wa_data2-irn        =  'GST'.
      wa_data2-suptyp     =  'B2B'.
      wa_data2-regrev     =  'N'.
      wa_data2-ecmgstin   =  ' '.
      "BUYER DETAILS
      wa_data2-b_state    =  buyeradd-b-region  .
      wa_data2-b_pos      =  buyeradd-b-region .
      wa_data2-b_pin      =  buyeradd-b-postalcode  .
     ENDIF .

      ELSE .

      wa_data2-taxsch     =  'GST'.
      wa_data2-version    =  '1.1'.
      wa_data2-irn        =  'GST'.
      wa_data2-suptyp     =  'B2B'.
      wa_data2-regrev     =  'N'.
      wa_data2-ecmgstin   =  ' '.
      "BUYER DETAILS
      wa_data2-b_state    =  buyeradd-b-region  .
      wa_data2-b_pos      =  buyeradd-b-region .
      wa_data2-b_pin      =  buyeradd-b-postalcode  .
   ENDIF .

DATA: lt_tax TYPE STANDARD TABLE OF I_BillingDocumentItem-TaxCode,
      lv_all_a0 TYPE abap_bool.

if bIll_head-DistributionChannel NE '17' .

 IF bIll_head-SDDocumentCategory EQ 'M' or bill_head-BillingDocumentType = 'F2'.

 IF bIll_head-BillingDocumentType = 'F2' OR bIll_head-BillingDocumentType = 'JSTO'.
 SELECT TaxCode FROM I_BillingDocumentItem WITH PRIVILEGED ACCESS
 WHERE BillingDocument = @bIll_head-BillingDocument GROUP BY TaxCode INTO TABLE @lt_tax.

  lv_all_a0 = abap_true.

  LOOP AT lt_tax INTO DATA(ls_tax).
    IF ls_tax <> 'A0'.
      lv_all_a0 = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_all_a0 = abap_true.
    wa_data2-i_typ = 'BOS'.
  ELSE.
    wa_data2-i_typ = 'INV'.
  ENDIF.

ELSE.

 wa_data2-i_typ = 'INV'.

ENDIF.

     if bIll_head-CustomerTaxClassification1 = '6'.
       wa_data2-suptyp     = 'SEZWOP'.
     else.
       wa_data2-suptyp     = 'B2B'.
     ENDIF.
  ELSEIF bIll_head-SDDocumentCategory = 'O'.
        wa_data2-i_typ = 'CRN'.
    wa_data2-suptyp     = 'B2B'.
 ELSEIF bIll_head-SDDocumentCategory = 'P'.
        wa_data2-i_typ = 'DBN'.
     wa_data2-suptyp     = 'B2B'.
 ELSEIF bIll_head-SDDocumentCategory = 'U'.
        wa_data2-i_typ = 'PINV'.
  ENDIF.

 ENDIF.

 if bIll_head-DistributionChannel = '17'  .

 IF bIll_head-SDDocumentCategory EQ 'M' or bill_head-BillingDocumentType = 'F2'.
        wa_data2-i_typ = 'INV'.
  ELSEIF bIll_head-SDDocumentCategory = 'O'.
        wa_data2-i_typ = 'CRN'.
  ELSEIF bIll_head-SDDocumentCategory = 'P'.
        wa_data2-i_typ = 'DBN'.
  ELSEIF bIll_head-SDDocumentCategory = 'U'.
        wa_data2-i_typ = 'PINV'.
   ENDIF.
  ENDIF.


    wa_data2-i_no       =  VBELN1 .
    SHIFT wa_data2-i_no  LEFT DELETING LEADING '0'.
    wa_data2-i_dt       =  inv_date."'20/09/2022' .
    wa_data2-totinvval  =  ' ' .

***********Supplier details I_supplier
 READ TABLE BILLING_ITEM into data(wa_bill) index 1.
 if SY-SYSID = 'ALU'  or SY-SYSID = 'AWL' .

    wa_data2-s_gstin    =  '08AAFCD5862R018' .
    wa_data2-s_lglnm    =  plant_add-AddresseeFullName.
    wa_data2-s_trdnm    =  plant_add-AddresseeFullName.
    wa_data2-s_addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } |.
    wa_data2-s_addr2    =  | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | .
    wa_data2-s_loc      =  plant_add-CityName.
    wa_data2-s_state    =  plant_add-Region .
    wa_data2-s_pin      =  plant_add-PostalCode.
    wa_data2-s_ph       =  ' '  .
    wa_data2-s_em       =  ' '  .


  ELSE.

 """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
 """"for production server""""""""
""""""""""""""""""""""""""""""""""""""""""""""""""""Plant Wise GST """""""""""""""""""""""""""""""""""

  SELECT single * FROM ZSD_PLANT_ADDRESS as a inner join I_BILLINGDOCUMENTITEMBASIC as b on ( a~Plant =  b~Plant )
                  where BillingDocument =  @vbeln1 AND a~plant = @wa_bill-Plant into @data(sellerplantaddress) .

   SELECT SINGLE * FROM zsales_tmg WHERE plant = @wa_bill-Plant INTO @data(gst).

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

    wa_data2-s_gstin    =  gst-gstno.
    wa_data2-s_lglnm    =  plant_add-AddresseeFullName.
    wa_data2-s_trdnm    =  plant_add-AddresseeFullName.
    wa_data2-s_addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } |.
    wa_data2-s_addr2    =   | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | .
    wa_data2-s_loc      =  plant_add-CityName.
    wa_data2-s_state    =  plant_add-Region .
    wa_data2-s_pin      =  plant_add-PostalCode .
    wa_data2-s_ph       =  ' '  .
    wa_data2-s_em       =  ' '  .
  ENDIF.


*************************Buyer details i_Customer details
 if buyeradd-b-Country eq 'IN' .
    wa_data2-b_gstin    =  buyeradd-b-TaxNumber3.
    wa_data2-b_lglnm    =  buyeradd-b-CustomerName.
    wa_data2-b_trdnm    =  buyeradd-b-CustomerName .
    wa_data2-b_addr1    =  |{ buyeradd-C-Street } { buyeradd-C-StreetName } { buyeradd-C-StreetPrefixName1 } { buyeradd-C-StreetPrefixName2 } { buyeradd-C-StreetSuffixName1 } { buyeradd-C-StreetSuffixName2 }|.
    wa_data2-b_addr2    =  |{ buyeradd-b-DistrictName } { buyeradd-k-RegionName } { buyeradd-m-CountryName } |.
    wa_data2-b_loc      =  buyeradd-b-CityName .
    wa_data2-b_state    =  buyeradd-b-Region  .
    wa_data2-b_pos      =  buyeradd-b-Region .
    wa_data2-b_pin      =  buyeradd-b-PostalCode  .
    wa_data2-b_ph       =  buyeradd-b-FaxNumber .
    wa_data2-b_em       =  ' ' .
 else .
    wa_data2-b_gstin    =  'URP'.
    wa_data2-b_lglnm    =  buyeradd-b-CustomerName.
    wa_data2-b_trdnm    =  buyeradd-b-CustomerName .
    wa_data2-b_addr1    =  |{ buyeradd-C-Street } { buyeradd-C-StreetName } { buyeradd-C-StreetPrefixName1 } { buyeradd-C-StreetPrefixName2 } { buyeradd-C-StreetSuffixName1 } { buyeradd-C-StreetSuffixName2 }|. .
    wa_data2-b_addr2    =  |{ buyeradd-b-DistrictName } { buyeradd-k-RegionName } { buyeradd-m-CountryName } |.
    wa_data2-b_loc      =  buyeradd-b-CityName .
    wa_data2-b_state    =  '96' .
    wa_data2-b_pos      =  '96' .
    wa_data2-b_pin      =  '999999'  .
    wa_data2-b_ph       =  buyeradd-b-FaxNumber .
    wa_data2-b_em       =  ' ' .
 endif .

*************************Dispatch details company address
 IF i_BillingPart-PartnerFunction = 'ZD'.
    wa_data2-d_nm       =  disp-C-AddresseeFullName.
    wa_data2-d_addr1    =  | { disp-c-StreetName } { disp-c-StreetPrefixName1 }  { disp-c-StreetPrefixName2 } | . " plantaddress-a-AddresseeFullName.
    wa_data2-d_addr2    =  | { disp-c-StreetSuffixName1 }  { disp-c-StreetSuffixName2 } { disp-c-DistrictName }  | . " plantaddress-a-streete.
    wa_data2-d_loc      =  disp-c-CityName.
    wa_data2-d_stcd     =  disp-c-Region.
    wa_data2-d_pin      =  disp-c-PostalCode.
 ELSE.
    wa_data2-d_nm       =  plant_add-AddresseeFullName.
    wa_data2-d_addr1    =  | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } | . " plantaddress-a-AddresseeFullName.
    wa_data2-d_addr2    =  | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } { plant_add-DistrictName }  | . " plantaddress-a-streete.
    wa_data2-d_loc      =  plant_add-CityName.
    wa_data2-d_stcd     =  plant_add-Region.
    wa_data2-d_pin      =  plant_add-PostalCode.
ENDIF.

*************************Shiping Details  "Ship to party address
 IF wa_data2-suptyp     =  'EXPWOP' OR wa_data2-suptyp   =  'EXPWP'.
    wa_data2-sh_gstin   =  'URP'.
    wa_data2-sh_stcd    =  StateCode.
    wa_data2-sh_pin     =  Postal.
    wa_data2-sh_lglnm   =  Shippingadd-b-CustomerName .
    wa_data2-sh_trdnm   =  Shippingadd-b-CustomerName .
    wa_data2-sh_addr1   =  |{ Shippingadd-C-Street } { Shippingadd-C-StreetName } { Shippingadd-C-StreetPrefixName1 } { Shippingadd-C-StreetPrefixName2 } { Shippingadd-C-StreetSuffixName1 } { Shippingadd-C-StreetSuffixName2 }|.
    wa_data2-sh_addr2   =  |{ Shippingadd-b-DistrictName } { Shippingadd-b-CityName } { Shippingadd-k-RegionName } { Shippingadd-m-CountryName }|  .
    wa_data2-sh_loc     =  LOC .
 ELSE.
    wa_data2-sh_gstin   =  Shippingadd-b-TaxNumber3 .
    wa_data2-sh_stcd    = Shippingadd-B-Region  .
    wa_data2-sh_pin     = Shippingadd-B-PostalCode  .
    wa_data2-sh_lglnm   =  Shippingadd-b-CustomerName .
    wa_data2-sh_trdnm   =  Shippingadd-b-CustomerName .
    wa_data2-sh_addr1   =  |{ Shippingadd-C-Street } { Shippingadd-C-StreetName } { Shippingadd-C-StreetPrefixName1 } { Shippingadd-C-StreetPrefixName2 } { Shippingadd-C-StreetSuffixName1 } { Shippingadd-C-StreetSuffixName2 }|. .
    wa_data2-sh_addr2   = |{ Shippingadd-b-DistrictName } { Shippingadd-k-RegionName } { Shippingadd-m-CountryName }|  .
    wa_data2-sh_loc     =  Shippingadd-b-CityName .
 ENDIF .


 loop at  BILLING_ITEM into DATA(bill_ITEM) .
data assamt type string.
data n type string.
n = n + 1.
SELECT SINGLE * FROM I_ProductDescription_2 WHERE Product = @bill_ITEM-Material and Language = 'E'  INTO @DATA(MAT_DES).
SELECT SINGLE * FROM ZMAT_TEXT WHERE Product = @bill_ITEM-Material and Plant = @bill_ITEM-Plant INTO @DATA(MATDES).
    WA_ITEMLIST1-slno       =  n.

  IF MATDES-mattext <> ''.
    WA_ITEMLIST1-prddesc    = MATDES-mattext .
  ELSE.
    WA_ITEMLIST1-prddesc    = MAT_DES-ProductDescription .
  ENDIF  .

    select single * from I_ProductPlantBasic as a left join I_Product as b on ( a~Product = b~Product )
     where a~Product = @BILL_ITEM-Material and a~plant = @BILL_ITEM-Plant  into @data(hsnSac).

   if bIll_head-Division = '91' AND bIll_head-DistributionChannel = '06' .

   WA_ITEMLIST1-isservc    =  'Y' .
   WA_ITEMLIST1-hsncd      =  '998821' .

   ELSEif hsnsac-b-ProductType = 'SERV'.

    WA_ITEMLIST1-isservc    =  'Y' .
    WA_ITEMLIST1-hsncd      =  hsnSac-A-ConsumptionTaxCtrlCode .

    elseif hsnsac-a-ConsumptionTaxCtrlCode = '998821'.

    WA_ITEMLIST1-isservc    =  'Y' .
    WA_ITEMLIST1-hsncd      =  hsnSac-A-ConsumptionTaxCtrlCode .
    ELSE.
    WA_ITEMLIST1-isservc    =  'N' .
    WA_ITEMLIST1-hsncd      =  hsnSac-A-ConsumptionTaxCtrlCode .
   ENDIF.

    WA_ITEMLIST1-barcde     =  ' ' .
    WA_ITEMLIST1-qty        =  BILL_ITEM-BillingQuantity .
  "  WA_ITEMLIST1-freeqty    =  BILL_ITEM-BillingQuantity .


            IF bill_item-billingquantityunit           = 'TO' .
        WA_ITEMLIST1-unit       =  'TON' .
      ELSEIF bill_item-billingquantityunit      = 'MTR' or bill_item-billingquantityunit      = 'M'.
        WA_ITEMLIST1-unit       =  'MTR' .
      ELSEIF bill_item-billingquantityunit     = 'KG' .
        WA_ITEMLIST1-unit       =  'KGS' .
        ELSEIF bill_item-billingquantityunit     = 'PC' OR  bill_item-billingquantityunit     = 'ST' .
        WA_ITEMLIST1-unit       =  'PCS' .
      ELSEIF bill_item-billingquantityunit+0(3) = 'BAG' .
        WA_ITEMLIST1-unit       =  'BAGS'.
      ELSEIF bill_item-billingquantityunit+0(3) = 'NO' .
        WA_ITEMLIST1-unit       =  'NOS'.
     ELSEIF bill_item-billingquantityunit+0(3) = 'PAK' .
        WA_ITEMLIST1-unit       =  'PAC'.
     ELSEIF bill_item-billingquantityunit+0(3) = 'CS' .
        WA_ITEMLIST1-unit       =  'CTN'.
      ELSE .
        WA_ITEMLIST1-unit        =  'OTH'.
      ENDIF .

"""""""""""""""""""""BASIC PRICE""""""""""""""""""""""""""
 select single conditionbaseamount from I_BILLINGDOCUMENTITEMPRCGELMNT where BillingDocument = @bill_ITEM-BillingDocument
                                     AND BillingDocumentItem = @bill_ITEM-BillingDocumentItem AND
 conditionbaseamount is not initial and  ConditionType in ( 'JOIG' ,'JOCG' ,'JOSG' ) INTO @DATA(BASEAMOUNT)  .

"""""""Added on 06/12/2025""""""

      READ TABLE b_item INTO DATA(zbasic) WITH KEY a-ConditionType  = 'ZASV'
                                                   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem.


      IF zbasic IS INITIAL.
      READ TABLE b_item INTO zbasic WITH KEY a-ConditionType        = 'ZR01'
                                                   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem.
      ENDIF.

       IF zbasic IS INITIAL.
       READ TABLE b_item INTO zbasic WITH KEY a-ConditionType        = 'ZR00'
                                                   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem.
      ENDIF.

     IF zbasic IS INITIAL.
          READ TABLE b_item INTO zbasic WITH KEY a-ConditionType        = 'PCIP'
                                                   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem.
     ENDIF.

""""""end""""""""""""""""""""""


""""""""comment 1 """""""""""""""

*       IF zbasic IS NOT INITIAL.
*          READ TABLE b_item INTO zbasic WITH KEY a-ConditionType        = 'ZR00'
*                                                   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem.
*      ENDIF.
  """"""end""""""""""""""""""""""


      wa_itemlist1-unitprice = zbasic-a-ConditionRateValue * bIll_head-AccountingExchangeRate.

"""""""Added on 06/12/2025""""""
   "   wa_itemlist1-totamt   = wa_itemlist1-unitprice * BILL_ITEM-BillingQuantity .
      wa_itemlist1-totamt   = zbasic-a-ConditionAmount * bIll_head-AccountingExchangeRate .   "wa_itemlist1-unitprice * BILL_ITEM-BillingQuantity .
""""""end""""""""""""""""""""""""

      IF baseamount IS NOT INITIAL.
      assamt =  baseamount * bIll_head-AccountingExchangeRate.
      ELSE.
      IF bill_item-TransactionCurrency = 'JPY'.
        assamt = ( zbasic-a-ConditionAmount * 100 ) * bIll_head-AccountingExchangeRate.
     ELSE.
      assamt = zbasic-a-ConditionAmount * bIll_head-AccountingExchangeRate.
     ENDIF.
      ENDIF.


      IF bIll_head-DistributionChannel = '17' .
        bIll_head-AccountingExchangeRate = bIll_head-AccountingExchangeRate.
        "   **************************************EXPORT EXCHANGE RATE LOGIC ******************8
        IF bill_item-TransactionCurrency = 'JPY'.
          wa_itemlist1-unitprice = zbasic-a-ConditionRateValue * bIll_head-AccountingExchangeRate / 100.
        ELSE.
          wa_itemlist1-unitprice = zbasic-a-ConditionRateValue * bIll_head-AccountingExchangeRate.
        ENDIF.
      ENDIF.

""""""""""""""""""""""""""""""""""""""discount"""""""""""""""""""""""""""""""""""""
      SELECT SUM( conditionamount ) FROM i_billingdocumentitemprcgelmnt
        WHERE ConditionType   IN ( 'ZFO1' ,'ZFO2' ,'ZFO3', 'ZCDI','ZCDE' )  "( 'ZDIS', 'ZDIH', 'ZSHE','ZDQT','ZFO1' ,'ZFO2' ,'ZFO3', 'ZCDI','ZCDE', )
          AND BillingDocument  = @bill_ITEM-BillingDocument AND BillingDocumentItem = @bill_ITEM-BillingDocumentItem
         INTO @DATA(discount).

         discount = discount *  bIll_head-AccountingExchangeRate .
         discount = ABS( discount ) .

      SELECT SUM( conditionamount ) FROM i_billingdocumentitemprcgelmnt
        WHERE ConditionType   IN ( 'ZPFA', 'ZINC', 'JTC1', 'JTC2' , 'ZTCS' )
          AND BillingDocument  = @bill_ITEM-BillingDocument AND BillingDocumentItem = @bill_ITEM-BillingDocumentItem
        INTO @DATA(otchg).

     WA_ITEMLIST1-othchrg    =   OTCHG  * bIll_head-AccountingExchangeRate .

*     IF  companycode = '1000' .
*     WA_ITEMLIST1-discount   =  discount .
*     WA_ITEMLIST1-assamt     = WA_ITEMLIST1-totamt .  "+ discount.

*     IF WA_ITEMLIST1-discount  < 0 .
*     WA_ITEMLIST1-discount = WA_ITEMLIST1-discount * -1.
*     ENDIF.

*     ELSE.
     WA_ITEMLIST1-discount   =  discount .
     WA_ITEMLIST1-assamt     = assamt .
   "  wa_itemlist1-totamt  = WA_ITEMLIST1-assamt.
*     ENDIF.

     WA_ITEMLIST1-othchrg    =  '0.0' .
     WA_ITEMLIST1-pretaxval  =  '0.0' .

"""""""""""""""""""""""""""""""""""""""""""""""TAX DATA*******************************
  read table B_ITEM into data(zbasiCIgst) with key a-ConditionType = 'JOIG'  a-BillingDocumentItem = bill_ITEM-BillingDocumentItem .
  IF SY-SUBRC EQ 0 .
    WA_ITEMLIST1-gstrt      =  zbasiCIgst-a-ConditionRateRatio .
    IF bill_item-TransactionCurrency = 'JPY'.
    WA_ITEMLIST1-igstamt    = ( zbasiCIgst-a-ConditionAmount * 100 )  * bIll_head-AccountingExchangeRate   .
    ELSE.
    WA_ITEMLIST1-igstamt    =  zbasiCIgst-a-ConditionAmount   * bIll_head-AccountingExchangeRate   .
    ENDIF.
    IGSTAMT  = IGSTAMT +  WA_ITEMLIST1-igstamt .

   ELSE.
     read table B_ITEM into DATA(zbasicgst) with key a-ConditionType = 'JOCG'  a-BillingDocumentItem = bill_ITEM-BillingDocumentItem .
        WA_ITEMLIST1-gstrt      =  zbasicgst-a-ConditionRateRatio * 2.
        WA_ITEMLIST1-cgstamt    =  zbasicgst-a-ConditionAmount  * bIll_head-AccountingExchangeRate   .
        CGSTAMT = CGSTAMT + WA_ITEMLIST1-cgstamt  .

     read table B_ITEM into DATA(zbasiSgst) with key a-ConditionType = 'JOSG'   a-BillingDocumentItem = bill_ITEM-BillingDocumentItem .
        WA_ITEMLIST1-sgstamt    =  zbasiSgst-a-ConditionAmount * bIll_head-AccountingExchangeRate .
        SGSTAMT = SGSTAMT + WA_ITEMLIST1-sgstamt     .

   ENDIF .
"""""""""""""""""""""""""""""""""""""""""""""""TAX DATA*******************************
     wa_itemlist1-othchrg      =    otchg  * bill_head-accountingexchangerate .
     WA_ITEMLIST1-cesrt              =  '0'  .
     WA_ITEMLIST1-cesamt             =  '0'  .
     WA_ITEMLIST1-statecesrt         =  '0'  .
     WA_ITEMLIST1-statecesamt        =  '0'  .
     WA_ITEMLIST1-statecesnonadvlamt =  '0'  .
     WA_ITEMLIST1-totitemval         =   WA_ITEMLIST1-assamt  +  WA_ITEMLIST1-igstamt + WA_ITEMLIST1-Cgstamt + WA_ITEMLIST1-Sgstamt + otchg .
     WA_ITEMLIST1-ordlineref         =  ' '  .
     WA_ITEMLIST1-orgcntry           =  ' '  .
     WA_ITEMLIST1-prdslno            =  ' '  .
     othamount   = othamount + WA_ITEMLIST1-othchrg .
     DATA TotalAssAMT TYPE P DECIMALS 2.
     TotalAssAMT = TotalAssAMT + WA_ITEMLIST1-assamt .
    Append WA_ITEMLIST1 TO ITEMLIST .
    clear :  WA_ITEMLIST1 ,discount,MAT_DES,discount,zbasic,BASEAMOUNT ,assamt.

    Endloop .

**************************AttribDtls Attribute
    wa_data2-a_nm        =  ' '  .
    wa_data2-a_val       =  ' '  .

    "Batch details{ }
    wa_data2-b_nm        =  ''  .
    wa_data2-b_expdt     =  ' '  .
    wa_data2-b_wrdt      =  ' '  .

**************************ValDtls Value details{final}
**************************ValDtls Value details{final}
DATA ZD02_AMTCHARGAE TYPE P DECIMALS 2.

IF  companycode = '1000' .

 SELECT SINGLE SUM( a~ConditionAmount ) as CondAmt FROM I_BillingDocumentPrcgElmnt WITH PRIVILEGED ACCESS as a
WHERE a~BillingDocument = @vbeln1 AND (
 a~ConditionType = 'ZD02'  )
INTO  @ZD02_AMTCHARGAE.
ENDIF.


 IF bill_head-distributionchannel = '17'.

   wa_data2-assval   =   TotalAssAMT  .

""""""""comment 2   -    06/12/2025"""""""""""""""

*  SELECT SUM( ConditionAmount ) FROM I_BillingDocumentItemPrcgElmnt AS A  INNER JOIN  I_BillingDocument AS B
*   ON ( A~BillingDocument = B~BillingDocument )  WHERE A~BillingDocument  = @vbeln1  AND
*   (  A~ConditionType = 'ZR00' OR A~ConditionType = 'ZR01' ) INTO  @DATA(ZFOC)   .
*
**  Read table ZFOC into data(FOC) with key a-ConditionType = 'ZR00' a-BillingDocument = bill_ITEM-BillingDocument .
* IF bill_item-TransactionCurrency = 'JPY'.
*  wa_data2-assval   =   ( ZFOC * 100 ) * bIll_head-AccountingExchangeRate  .
*  ELSE .
*   wa_data2-assval   =   ZFOC * bIll_head-AccountingExchangeRate .
*  ENDIF.

  """"""end""""""""""""""""""""""

 ELSE.
    wa_data2-assval      =   TotalAssAMT  * bIll_head-AccountingExchangeRate .
  ENDIF.


  SELECT SUM( ConditionAmount ) FROM I_BillingDocumentItemPrcgElmnt AS A  INNER JOIN  I_BillingDocument AS B
   ON ( A~BillingDocument = B~BillingDocument )  WHERE A~BillingDocument  = @vbeln1  AND
   (  A~ConditionType = 'ZDOF' ) INTO  @DATA(ZDOF)   .

    wa_data2-cgstval     =   CGSTAMT  .
    wa_data2-sgstval     =   SGSTAMT  .
    wa_data2-igstval     =   IGSTAMT .
    wa_data2-cesval      =   '0.0'  .
    wa_data2-stcesval    =   '0.0'  .

     wa_data2-othchrg     =   othamount .

    wa_data2-rndoffamt   =   ZDOF  .
    wa_data2-totinvval   =   wa_data2-assval + CGSTAMT + SGSTAMT +  IGSTAMT  + othamount + ZDOF.
    wa_data2-totinvvalfc =   '0.0 ' .

    "Payment Details
    wa_data2-p_nm       =  ' '  .
    wa_data2-p_mode     =  ' '  .
    wa_data2-p_fininsbr =  ' '  .
    wa_data2-p_payterm  =  ' '  .
    wa_data2-p_payinstr =  ' '  .
    wa_data2-p_crtrn    =  ' '  .
    wa_data2-p_dirdr    =  ' '  .
    wa_data2-p_crday    =  ' '  .
    wa_data2-p_paidamt  =  '0.0' .
    wa_data2-p_paymtdue =  '0.0' .
    wa_data2-p_accdet   =   ' '  .

*************************RefDtls  refrence details  "Invoice remarks
    wa_data2-invrm      =   wa_data2-i_no .

*************************DocPerdDtls Perceding Details
    wa_data2-invstdt    =  |{ inv_date+6(2) }/{ inv_date+4(2) }/{ inv_date+0(4) }|  .
    wa_data2-invenddt   =  |{ inv_date+6(2) }/{ inv_date+4(2) }/{ inv_date+0(4) }|   .

*************************PRECDocDtls
    wa_data2-p_invno    =   wa_data2-i_no."bIll_head-DocumentReferenceID   .
    wa_data2-p_invdt    =   |{ inv_date+6(2) }/{ inv_date+4(2) }/{ inv_date+0(4) }|  .
    wa_data2-othrefno   =  ' '  .

*************************ContrDtls
    wa_data2-recadvrefr =  ' '  .
    wa_data2-recadvdt   =  ' '  .
    wa_data2-tendrefr   =  ' '  .
    wa_data2-contrrefr  =  ' '  .
    wa_data2-extrefr    =  ' '  .
    wa_data2-projrefr   =  ' '  .
    wa_data2-porefr     =  ' '  .
    wa_data2-porefdt    =  ' '  .

*************************ADDLDocDtls
    wa_data2-url        =  ' '  .
    wa_data2-docs       =  ' '  .
    wa_data2-info       =  ' '  .

***************************ExpDtls
    wa_data2-shipbno      =   ' '  .
    wa_data2-shipbdt      =   ' '  .
*    wa_data2-port         =   port  .
    wa_data2-refclm       =   ' '  .
    wa_data2-totinvvalfc  =   ' ' .
    wa_data2-forcur       =   bIll_head-TransactionCurrency  .
    wa_data2-cntcode      =   buyeradd-b-Country .
    wa_data2-expduty      =   ' '  .

 DATA GV TYPE STRING .
 Data  trsprt type Ytransport_det .
*************************EwbDtls
    wa_data2-transid    =   transportid  .
    wa_data2-transname  =   transpoter_name .

 if irngenrate = 'X' and eway_generate = 'X'  .
      if SHIP-ShippingType <> ''.
      SHIFT SHIP-ShippingType LEFT DELETING LEADING '0' .
      else.
      SHIP-ShippingType = '1'.
      ENDIF.
 ELSE.
  SHIP-ShippingType = ' '.
 ENDIF.

    wa_data2-transmode  =   ship-ShippingType ."|{ bIll_head-YY1_TransportMode_BDH ALPHA = out }| .
    wa_data2-distance   =   distance  .
    wa_data2-transdocno =   bIll_head-yy1_lrno1_bdh.
   if  bIll_head-YY1_LRDate_BDH <> '00000000'.
    wa_data2-transdocdt =   |{ bIll_head-YY1_LRDate_BDH+6(2) }/{ bIll_head-YY1_LRDate_BDH+4(2) }/{ bIll_head-YY1_LRDate_BDH+0(4) }| .
   endiF.
    wa_data2-vehno      =   bIll_head-YY1_VehicleNo_BDH  .
    if bIll_head-YY1_VehicleNo_BDH is INITIAL.
    wa_data2-vehno      =   vehiclenumber  .
    ENDIF.

if SHIP-ShippingType is INITIAL.
    wa_data2-vehtype    =   ''  .
else.
    wa_data2-vehtype    =   'R'  .
endif.

    wa_data2-prctr      =   ' '  .
    wa_data2-distance1  =   ' '  .

    APPEND wa_data2  TO it_data2 .

    LOOP AT it_data2 INTO DATA(wa_final) .

      wa_json_data-version   =  wa_final-version .
      wa_json_data2-version  =  wa_final-version .
      wa_json_data3-version  =  wa_final-version .
      wa_json_data4-version  =  wa_final-version .
      docdtls-typ  = wa_data2-i_typ .
      docdtls-no  =  wa_final-i_no .
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

       docdtls-dt  = dateformat( date = wa_final-i_dt ) .

************************Transport data
      trandtls-taxsch   = wa_final-taxsch.
      trandtls-suptyp   = wa_final-suptyp.
      trandtls-regrev   = wa_final-regrev.
      trandtls-ecmgstin = wa_final-ecmgstin.

************************Seller_Data
      sellerdtls-gstin = wa_final-s_gstin.
      sellerdtls-lglnm = wa_final-s_lglnm.
      sellerdtls-trdnm = wa_final-s_trdnm.
      sellerdtls-addr1 = wa_final-s_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN sellerdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN sellerdtls-addr1 WITH ' ' .
      sellerdtls-addr2 = wa_final-s_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN sellerdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN sellerdtls-addr2 WITH ' ' .
      sellerdtls-loc = wa_final-s_loc.
      sellerdtls-pin = wa_final-s_pin.
      sellerdtls-stcd = wa_final-s_state.
      sellerdtls-ph = wa_final-s_ph .
      sellerdtls-em = wa_final-s_em .

***********************Buyer details
      buyerdtls-gstin = wa_final-b_gstin.
      buyerdtls-lglnm = wa_final-b_lglnm.
      buyerdtls-trdnm = wa_final-b_trdnm.
      buyerdtls-pos   = wa_final-b_pos.
      buyerdtls-addr1 = wa_final-b_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN  buyerdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN  buyerdtls-addr1 WITH ' ' .
      buyerdtls-addr2 = wa_final-b_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN  buyerdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN  buyerdtls-addr2 WITH ' ' .
      buyerdtls-loc   = wa_final-b_loc .
      buyerdtls-pin   = wa_final-b_pin .
     IF buyerdtls-pin = '0'.
        buyerdtls-pin = '111111'.
      ENDIF.
      buyerdtls-stcd  = wa_final-b_state.
      buyerdtls-ph    = wa_final-b_ph.
      buyerdtls-em    = wa_final-b_em.

*************************Dispatch details
      dispdtls-nm       = wa_final-d_nm.
      dispdtls-addr1    = wa_final-d_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN dispdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN dispdtls-addr1 WITH ' ' .
      dispdtls-addr2    = wa_final-d_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN dispdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN dispdtls-addr2 WITH ' ' .
      dispdtls-loc      = wa_final-d_loc.
      dispdtls-pin      = wa_final-d_pin.
      dispdtls-stcd     = wa_final-d_stcd.

      "shiping details

      shipdtls-gstin = wa_final-sh_gstin.
      shipdtls-lglnm = wa_final-sh_lglnm.
      shipdtls-trdnm = wa_final-sh_trdnm.
      shipdtls-addr1 = wa_final-sh_addr1.
      REPLACE ALL OCCURRENCES OF ',' IN shipdtls-addr1 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN shipdtls-addr1 WITH ' ' .
      shipdtls-addr2 = wa_final-sh_addr2.
      REPLACE ALL OCCURRENCES OF ',' IN shipdtls-addr2 WITH ' ' .
      REPLACE ALL OCCURRENCES OF '’' IN shipdtls-addr2 WITH ' ' .
      shipdtls-loc   = wa_final-sh_loc.
      shipdtls-pin   = wa_final-sh_pin.
      shipdtls-stcd  = wa_final-sh_stcd.



 loop at itemlist into wa_itemlist  .
 APPEND wa_itemlist TO wa_json_data-itemlist .
 APPEND wa_itemlist TO wa_json_data2-itemlist .
 APPEND wa_itemlist TO wa_json_data3-itemlist .
 APPEND wa_itemlist TO wa_json_data4-itemlist .
 clear wa_itemlist .
 endloop .

      attribdtls-nm = wa_final-a_nm.
      attribdtls-val = wa_final-a_val.
      DATA:assval TYPE string.
      DATA:inv    TYPE string.
      DATA:invfc  TYPE string.
      DATA:oth    TYPE string.
      DATA:sgst   TYPE string.
      DATA:cgst   TYPE string.
      DATA:igst   TYPE string.
      assval = assval + wa_final-assval.

      valdtls-assval = assval.

      cgst = cgst + wa_final-cgstval.
      sgst = sgst + wa_final-sgstval.
      igst = igst + wa_final-igstval.
      valdtls-cgstval  = cgst.
      valdtls-sgstval  = sgst.
      valdtls-igstval  = igst.

     valdtls-cesval    = wa_final-cesval.
     valdtls-stcesval  = wa_final-stcesval.
     valdtls-rndoffamt = wa_final-rndoffamt.
      invfc = invfc +  wa_final-totinvvalfc.

      valdtls-totinvvalfc = invfc .
      inv = inv + wa_final-totinvval .
      valdtls-totinvval   = inv.

      paydtls-nm       = wa_final-p_nm.
      paydtls-accdet   = wa_final-p_accdet.
      paydtls-mode     = wa_final-p_mode.
      paydtls-fininsbr = wa_final-p_fininsbr.
      paydtls-payterm  = wa_final-p_payterm.
      paydtls-payinstr = wa_final-p_payinstr.
      paydtls-crtrn    = wa_final-p_crtrn.
      paydtls-dirdr    = wa_final-p_dirdr.
      paydtls-crday    = wa_final-p_crday.
      paydtls-paidamt  = wa_final-p_paidamt.
      paydtls-paymtdue = wa_final-p_paymtdue.

      refdtls-invrm    = wa_data2-i_no .
      docpre-invstdt   = wa_final-invstdt.

      docpre-invenddt      = wa_final-invenddt.
      precdocdtls-invno    = wa_final-p_invno.
      precdocdtls-invdt    = wa_final-p_invdt.
      precdocdtls-othrefno = wa_final-othrefno.

      contrdtls-recadvrefr = wa_final-recadvrefr.
      contrdtls-recadvdt   = wa_final-recadvdt.
      contrdtls-tendrefr   = wa_final-tendrefr.
      contrdtls-contrrefr  = wa_final-contrrefr.
      contrdtls-extrefr    = wa_final-extrefr.
      contrdtls-projrefr   = wa_final-projrefr.
      contrdtls-porefr     = wa_final-porefr.
      contrdtls-porefdt    = wa_final-porefdt.

     addldocdtls-url  = wa_final-url.
     addldocdtls-docs = wa_final-docs.

      expdtls-shipbno = wa_final-shipbno.
      expdtls-shipbdt = wa_final-shipbdt.
      expdtls-port    = wa_final-port.
      expdtls-refclm  = wa_final-refclm.
      expdtls-forcur  = wa_final-forcur.
      expdtls-cntcode = wa_final-cntcode.

      bchdtls-nm    = wa_final-b_nm.
      bchdtls-expdt = wa_final-b_expdt.
      bchdtls-wrdt  = wa_final-b_wrdt.


************************************IF eway_generate = 'X'.
      ewbdtls-transid   = wa_final-transid.
      ewbdtls-transname = wa_final-transname.
      ewbdtls-transmode = wa_final-transmode.
      EWBDTLS-vehno     = WA_FINAL-vehno.
      EWBDTLS-distance  = WA_FINAL-distance.

      DATA: del1 TYPE string,
            del2 TYPE string.

      DATA : dest TYPE string .
      dest  =    wa_final-distance  .
      SPLIT dest AT '.' INTO del1 del2.
      ewbdtls-distance = del1."WA_FINAL-DISTANCE.
      IF del1 IS INITIAL.
        ewbdtls-distance = '0'.
      ENDIF.
      ewbdtls-transdocno = wa_final-transdocno.
      ewbdtls-transdocdt = wa_final-transdocdt.
      REPLACE ALL OCCURRENCES OF '.' IN ewbdtls-transdocdt WITH '/'.
      ewbdtls-vehno = wa_final-vehno.
      ewbdtls-vehtype = wa_final-vehtype.
************************************IF eway_generate = 'X'.

IF     ( wa_data2-d_pin      =  wa_data2-s_pin  ) AND ( wa_data2-b_pin  = wa_data2-sh_pin ) .

      MOVE-CORRESPONDING trandtls TO wa_json_data-trandtls.
      MOVE-CORRESPONDING docdtls TO wa_json_data-docdtls.
      MOVE-CORRESPONDING sellerdtls TO wa_json_data-sellerdtls.
      MOVE-CORRESPONDING buyerdtls TO wa_json_data-buyerdtls.
      MOVE-CORRESPONDING attribdtls TO wa_json_data-attribdtls.
      MOVE-CORRESPONDING valdtls TO wa_json_data-valdtls.
      MOVE-CORRESPONDING paydtls TO wa_json_data-paydtls.
      MOVE-CORRESPONDING precdocdtls TO wa_json_data-precdocdtls.
      MOVE-CORRESPONDING contrdtls TO wa_json_data-contrdtls.
      MOVE-CORRESPONDING refdtls TO wa_json_data-refdtls.
      MOVE-CORRESPONDING docpre TO  wa_json_data-docpre.
      MOVE-CORRESPONDING addldocdtls TO wa_json_data-addldocdtls.
      MOVE-CORRESPONDING expdtls TO wa_json_data-expdtls.
      MOVE-CORRESPONDING bchdtls TO wa_json_data-bchdtls.
      MOVE-CORRESPONDING ewbdtls TO wa_json_data-ewbdtls.

      APPEND wa_json_data TO it_json_data.

ELSEIF ( wa_data2-d_pin      =  wa_data2-s_pin ) AND ( wa_data2-b_pin <>  wa_data2-sh_pin ) .

      MOVE-CORRESPONDING trandtls TO wa_json_data2-trandtls.
      MOVE-CORRESPONDING docdtls TO wa_json_data2-docdtls.
      MOVE-CORRESPONDING sellerdtls TO wa_json_data2-sellerdtls.
      MOVE-CORRESPONDING buyerdtls TO wa_json_data2-buyerdtls.
      MOVE-CORRESPONDING shipdtls TO wa_json_data2-shipdtls.
      MOVE-CORRESPONDING attribdtls TO wa_json_data2-attribdtls.
      MOVE-CORRESPONDING valdtls TO wa_json_data2-valdtls.
      MOVE-CORRESPONDING paydtls TO wa_json_data2-paydtls.
      MOVE-CORRESPONDING precdocdtls TO wa_json_data2-precdocdtls.
      MOVE-CORRESPONDING contrdtls TO wa_json_data2-contrdtls.
      MOVE-CORRESPONDING refdtls TO wa_json_data2-refdtls.
      MOVE-CORRESPONDING docpre TO  wa_json_data2-docpre.
      MOVE-CORRESPONDING addldocdtls TO wa_json_data2-addldocdtls.
      MOVE-CORRESPONDING expdtls TO wa_json_data2-expdtls.
      MOVE-CORRESPONDING bchdtls TO wa_json_data2-bchdtls.
      MOVE-CORRESPONDING ewbdtls TO wa_json_data2-ewbdtls.

      APPEND wa_json_data2 TO it_json_data2.

ELSEIF ( wa_data2-d_pin      <>  wa_data2-s_pin ) AND ( wa_data2-b_pin  = wa_data2-sh_pin ) .

      MOVE-CORRESPONDING trandtls TO wa_json_data3-trandtls.
      MOVE-CORRESPONDING docdtls TO wa_json_data3-docdtls.
      MOVE-CORRESPONDING sellerdtls TO wa_json_data3-sellerdtls.
      MOVE-CORRESPONDING buyerdtls TO wa_json_data3-buyerdtls.
      MOVE-CORRESPONDING dispdtls TO wa_json_data3-dispdtls.
      MOVE-CORRESPONDING attribdtls TO wa_json_data3-attribdtls.
      MOVE-CORRESPONDING valdtls TO wa_json_data3-valdtls.
      MOVE-CORRESPONDING paydtls TO wa_json_data3-paydtls.
      MOVE-CORRESPONDING precdocdtls TO wa_json_data3-precdocdtls.
      MOVE-CORRESPONDING contrdtls TO wa_json_data3-contrdtls.
      MOVE-CORRESPONDING refdtls TO wa_json_data3-refdtls.
      MOVE-CORRESPONDING docpre TO  wa_json_data3-docpre.
      MOVE-CORRESPONDING addldocdtls TO wa_json_data3-addldocdtls.
      MOVE-CORRESPONDING expdtls TO wa_json_data3-expdtls.
      MOVE-CORRESPONDING bchdtls TO wa_json_data3-bchdtls.
      MOVE-CORRESPONDING ewbdtls TO wa_json_data3-ewbdtls.

      APPEND wa_json_data3 TO it_json_data3.

ELSEIF ( wa_data2-d_pin      <>  wa_data2-s_pin ) AND ( wa_data2-b_pin  <> wa_data2-sh_pin ) .

      MOVE-CORRESPONDING trandtls TO wa_json_data4-trandtls.
      MOVE-CORRESPONDING docdtls TO wa_json_data4-docdtls.
      MOVE-CORRESPONDING sellerdtls TO wa_json_data4-sellerdtls.
      MOVE-CORRESPONDING buyerdtls TO wa_json_data4-buyerdtls.
      MOVE-CORRESPONDING dispdtls TO wa_json_data4-dispdtls.
      MOVE-CORRESPONDING shipdtls TO wa_json_data4-shipdtls.
      MOVE-CORRESPONDING attribdtls TO wa_json_data4-attribdtls.
      MOVE-CORRESPONDING valdtls TO wa_json_data4-valdtls.
      MOVE-CORRESPONDING paydtls TO wa_json_data4-paydtls.
      MOVE-CORRESPONDING precdocdtls TO wa_json_data4-precdocdtls.
      MOVE-CORRESPONDING contrdtls TO wa_json_data4-contrdtls.
      MOVE-CORRESPONDING refdtls TO wa_json_data4-refdtls.
      MOVE-CORRESPONDING docpre TO  wa_json_data4-docpre.
      MOVE-CORRESPONDING addldocdtls TO wa_json_data4-addldocdtls.
      MOVE-CORRESPONDING expdtls TO wa_json_data4-expdtls.
      MOVE-CORRESPONDING bchdtls TO wa_json_data4-bchdtls.
      MOVE-CORRESPONDING ewbdtls TO wa_json_data4-ewbdtls.

      APPEND wa_json_data4 TO it_json_data4.

ENDIF.

      CLEAR: wa_final, trandtls, docdtls, sellerdtls, buyerdtls, dispdtls, shipdtls, wa_itemlist,
            bchdtls, valdtls, paydtls, refdtls, addldocdtls, expdtls, ewbdtls.

    ENDLOOP .

    DATA(json_writer) = cl_sxml_string_writer=>create(
                            type = if_sxml=>co_xt_json ).


IF     ( wa_data2-d_pin      =  wa_data2-s_pin  ) AND ( wa_data2-b_pin  = wa_data2-sh_pin ) .

         CALL TRANSFORMATION id SOURCE result = it_json_data
                           RESULT XML json_writer.
ELSEIF ( wa_data2-d_pin      =  wa_data2-s_pin ) AND ( wa_data2-b_pin <>  wa_data2-sh_pin ) .

         CALL TRANSFORMATION id SOURCE result = it_json_data2
                           RESULT XML json_writer.
ELSEIF ( wa_data2-d_pin      <>  wa_data2-s_pin ) AND ( wa_data2-b_pin  = wa_data2-sh_pin ) .

         CALL TRANSFORMATION id SOURCE result = it_json_data3
                           RESULT XML json_writer.
ELSEIF ( wa_data2-d_pin      <>  wa_data2-s_pin ) AND ( wa_data2-b_pin  <> wa_data2-sh_pin ) .

         CALL TRANSFORMATION id SOURCE result = it_json_data4
                           RESULT XML json_writer.

ENDIF.

    DATA(jsonx) = json_writer->get_output( ).
    DATA: lv_json TYPE string.
    DATA :lv_xstring_var TYPE xstring,
          strc           TYPE string.

    DATA(lv_string) = xco_cp=>xstring( jsonx
      )->as_string( xco_cp_character=>code_page->utf_8
      )->value.

    REPLACE ALL OCCURRENCES OF 'VERSION' IN lv_string WITH 'Version'.
    REPLACE ALL OCCURRENCES OF 'IRN' IN lv_string WITH 'Irn'.
    REPLACE ALL OCCURRENCES OF 'TRANDTLS' IN lv_string WITH 'TranDtls'.
    REPLACE ALL OCCURRENCES OF 'DOCDTLS' IN lv_string WITH 'DocDtls'.
    REPLACE ALL OCCURRENCES OF 'SELLERDTLS' IN lv_string WITH 'SellerDtls'.
    REPLACE ALL OCCURRENCES OF 'BUYERDTLS' IN lv_string WITH 'BuyerDtls'.
    REPLACE ALL OCCURRENCES OF 'DISPDTLS' IN lv_string WITH 'DispDtls'.
    REPLACE ALL OCCURRENCES OF 'SHIPDTLS' IN lv_string WITH 'ShipDtls'.
    REPLACE ALL OCCURRENCES OF 'ITEMLIST' IN lv_string WITH 'ItemList'.
    REPLACE ALL OCCURRENCES OF 'VALDTLS' IN lv_string WITH 'ValDtls'.
    REPLACE ALL OCCURRENCES OF 'PAYDTLS' IN lv_string WITH 'PayDtls'.
    REPLACE ALL OCCURRENCES OF 'REFDTLS' IN lv_string WITH 'RefDtls'.
    REPLACE ALL OCCURRENCES OF 'ADDLDOCDTLS' IN lv_string WITH 'AddlDocDtls'.
    REPLACE ALL OCCURRENCES OF 'EXPDTLS' IN lv_string WITH 'ExpDtls'.
    REPLACE ALL OCCURRENCES OF 'EWBDTLS' IN lv_string WITH 'EwbDtls'.
    REPLACE ALL OCCURRENCES OF 'TAXSCH' IN lv_string WITH 'TaxSch'.
    REPLACE ALL OCCURRENCES OF 'SUPTYP' IN lv_string WITH 'SupTyp'.
    REPLACE ALL OCCURRENCES OF 'TRANSACTIONTYPE' IN lv_string WITH 'TransactionType'.
    REPLACE ALL OCCURRENCES OF 'REGREV' IN lv_string WITH 'RegRev'.
    REPLACE ALL OCCURRENCES OF 'ECMGSTIN' IN lv_string WITH 'EcmGstin'.
    REPLACE ALL OCCURRENCES OF 'TYP' IN lv_string WITH 'Typ'.
    REPLACE ALL OCCURRENCES OF 'NO' IN lv_string WITH 'No'.
    REPLACE ALL OCCURRENCES OF 'DT' IN lv_string WITH 'Dt'.
    REPLACE ALL OCCURRENCES OF 'GSTIN' IN lv_string WITH 'Gstin'.
    REPLACE ALL OCCURRENCES OF 'LGLNM' IN lv_string WITH 'LglNm'.
    REPLACE ALL OCCURRENCES OF 'TRDNM' IN lv_string WITH 'TrdNm'.
    REPLACE ALL OCCURRENCES OF 'ADDR1' IN lv_string WITH 'Addr1'.
    REPLACE ALL OCCURRENCES OF 'ADDR2' IN lv_string WITH 'Addr2'.
    REPLACE ALL OCCURRENCES OF 'LOC' IN lv_string WITH 'Loc'.
    REPLACE ALL OCCURRENCES OF 'PIN' IN lv_string WITH 'Pin'.
    REPLACE ALL OCCURRENCES OF 'STATE' IN lv_string WITH 'State'.
    REPLACE ALL OCCURRENCES OF 'PH' IN lv_string WITH 'Ph'.
    REPLACE ALL OCCURRENCES OF 'EM' IN lv_string WITH 'Em'.
    REPLACE ALL OCCURRENCES OF 'POS' IN lv_string WITH 'Pos'.
    REPLACE ALL OCCURRENCES OF 'NM' IN lv_string WITH 'Nm'.
    REPLACE ALL OCCURRENCES OF 'STCD' IN lv_string WITH 'Stcd'.
    REPLACE ALL OCCURRENCES OF 'SLNO' IN lv_string WITH 'SlNo'.
    REPLACE ALL OCCURRENCES OF 'PRDDESC' IN lv_string WITH 'PrdDesc'.
    REPLACE ALL OCCURRENCES OF 'ISSERVC' IN lv_string WITH 'IsServc'.
    REPLACE ALL OCCURRENCES OF 'HSNCD' IN lv_string WITH 'HsnCd'.
    REPLACE ALL OCCURRENCES OF 'BCHDTLS' IN lv_string WITH 'BchDtls'.
    REPLACE ALL OCCURRENCES OF 'BARCDE' IN lv_string WITH 'Barcde'.
    REPLACE ALL OCCURRENCES OF 'QTY' IN lv_string WITH 'Qty'.
    REPLACE ALL OCCURRENCES OF 'FREEQTY' IN lv_string WITH 'FreeQty'.
    REPLACE ALL OCCURRENCES OF 'UNIT' IN lv_string WITH 'Unit'.
*  REPLACE ALL OCCURRENCES OF 'UNITPRICE' IN lv_string WITH 'UnitPrice'.
    REPLACE ALL OCCURRENCES OF 'TOTAMT' IN lv_string WITH 'TotAmt'.
    REPLACE ALL OCCURRENCES OF 'DISCOUNT' IN lv_string WITH 'Discount'.
    REPLACE ALL OCCURRENCES OF 'PRETAXVAL' IN lv_string WITH 'PreTaxVal'.
    REPLACE ALL OCCURRENCES OF 'ASSAMT' IN lv_string WITH 'AssAmt'.
    REPLACE ALL OCCURRENCES OF 'GSTRT' IN lv_string WITH 'GstRt'.
    REPLACE ALL OCCURRENCES OF 'IGSTAMT' IN lv_string WITH 'IgstAmt'.
    REPLACE ALL OCCURRENCES OF 'CGSTAMT' IN lv_string WITH 'CgstAmt'.
    REPLACE ALL OCCURRENCES OF 'SGSTAMT' IN lv_string WITH 'SgstAmt'.
    REPLACE ALL OCCURRENCES OF 'CESRT' IN lv_string WITH 'CesRt'.
    REPLACE ALL OCCURRENCES OF 'CESAMT' IN lv_string WITH 'CesAmt'.
    REPLACE ALL OCCURRENCES OF 'CESNONADVLAMT' IN lv_string WITH 'CesNonAdvlAmt'.
    REPLACE ALL OCCURRENCES OF 'STATECESRT' IN lv_string WITH 'StateCesRt'.
    REPLACE ALL OCCURRENCES OF 'STATECESAMT' IN lv_string WITH 'StateCesAmt'.
    REPLACE ALL OCCURRENCES OF 'STATECESNONADVLAMT' IN lv_string WITH 'StateCesNonAdvlAmt'.
    REPLACE ALL OCCURRENCES OF 'OTHCHRG' IN lv_string WITH 'OthChrg'.
    REPLACE ALL OCCURRENCES OF 'TOTITEMVAL' IN lv_string WITH 'TotItemVal'.
    REPLACE ALL OCCURRENCES OF 'ORDLINEREF' IN lv_string WITH 'OrdLineRef'.
    REPLACE ALL OCCURRENCES OF 'ORGCNTRY' IN lv_string WITH 'OrgCntry'.
    REPLACE ALL OCCURRENCES OF 'PRDSLNO' IN lv_string WITH 'PrdSlNo'.
    REPLACE ALL OCCURRENCES OF 'ATTRIBDTLS' IN lv_string WITH 'AttribDtls'.
    REPLACE ALL OCCURRENCES OF 'VAL' IN lv_string WITH 'Val'.
    REPLACE ALL OCCURRENCES OF 'EXPDT' IN lv_string WITH 'ExpDt'.
    REPLACE ALL OCCURRENCES OF 'WRDT' IN lv_string WITH 'WrDt'.
    REPLACE ALL OCCURRENCES OF 'ASSVAL' IN lv_string WITH 'AssVal'.
    REPLACE ALL OCCURRENCES OF 'CGSTVAL' IN lv_string WITH 'CgstVal'.
    REPLACE ALL OCCURRENCES OF 'SGSTVAL' IN lv_string WITH 'SgstVal'.
    REPLACE ALL OCCURRENCES OF 'IGSTVAL' IN lv_string WITH 'IgstVal'.
    REPLACE ALL OCCURRENCES OF 'CESVAL' IN lv_string WITH 'CesVal'.
    REPLACE ALL OCCURRENCES OF 'STCESVAL' IN lv_string WITH 'StCesVal'.
    REPLACE ALL OCCURRENCES OF 'RNDOFFAMT' IN lv_string WITH 'RndOffAmt'.
    REPLACE ALL OCCURRENCES OF 'TOTINVVALFC' IN lv_string WITH 'TotInvValFc'.
    REPLACE ALL OCCURRENCES OF 'ACCDET' IN lv_string WITH 'AccDet'.
    REPLACE ALL OCCURRENCES OF 'MODE' IN lv_string WITH 'Mode'.
    REPLACE ALL OCCURRENCES OF 'FININSBR' IN lv_string WITH 'FinInsBr'.
    REPLACE ALL OCCURRENCES OF 'PAYTERM' IN lv_string WITH 'PayTerm'.
    REPLACE ALL OCCURRENCES OF 'PAYINSTR' IN lv_string WITH 'PayInstr'.
    REPLACE ALL OCCURRENCES OF 'CRTRN' IN lv_string WITH 'CrTrn'.
    REPLACE ALL OCCURRENCES OF 'DIRDR' IN lv_string WITH 'DirDr'.
    REPLACE ALL OCCURRENCES OF 'CRDAY' IN lv_string WITH 'CrDay'.
    REPLACE ALL OCCURRENCES OF 'PAIDAMT' IN lv_string WITH 'PaidAmt'.
    REPLACE ALL OCCURRENCES OF 'PAYMTDUE' IN lv_string WITH 'PaymtDue'.
    REPLACE ALL OCCURRENCES OF 'INVRM' IN lv_string WITH 'InvRm'.
    REPLACE ALL OCCURRENCES OF 'INVSTDT' IN lv_string WITH 'InvStDt'.
    REPLACE ALL OCCURRENCES OF 'INVENDDTT' IN lv_string WITH 'InvEndDt'.
    REPLACE ALL OCCURRENCES OF 'PRECDOCDTLS' IN lv_string WITH 'PrecDocDtls'.
    REPLACE ALL OCCURRENCES OF 'CONTRDTLS' IN lv_string WITH 'ContrDtls'.
    REPLACE ALL OCCURRENCES OF 'INVNO' IN lv_string WITH 'InvNo'.
    REPLACE ALL OCCURRENCES OF 'INVDT' IN lv_string WITH 'InvDt'.
    REPLACE ALL OCCURRENCES OF 'OTHREFNO' IN lv_string WITH 'OthRefNo'.
    REPLACE ALL OCCURRENCES OF 'RECADVREFR' IN lv_string WITH 'RecAdvRefr'.
    REPLACE ALL OCCURRENCES OF 'RECADVDT' IN lv_string WITH 'RecAdvDt'.
    REPLACE ALL OCCURRENCES OF 'TENDREFR' IN lv_string WITH 'TendRefr'.
    REPLACE ALL OCCURRENCES OF 'CONTRREFR' IN lv_string WITH 'ContrRefr'.
    REPLACE ALL OCCURRENCES OF 'EXTREFR' IN lv_string WITH 'ExtRefr'.
    REPLACE ALL OCCURRENCES OF 'PROJREFR' IN lv_string WITH 'ProjRefr'.
    REPLACE ALL OCCURRENCES OF 'POREFR' IN lv_string WITH 'PORefr'.
    REPLACE ALL OCCURRENCES OF 'POREFDT' IN lv_string WITH 'PORefDt'.
    REPLACE ALL OCCURRENCES OF 'URL' IN lv_string WITH 'Url'.
    REPLACE ALL OCCURRENCES OF 'DOCS' IN lv_string WITH 'Docs'.
    REPLACE ALL OCCURRENCES OF 'INFO' IN lv_string WITH 'Info'.
    REPLACE ALL OCCURRENCES OF 'SHIPBNO' IN lv_string WITH 'ShipBNo'.
    REPLACE ALL OCCURRENCES OF 'SHIPBDT' IN lv_string WITH 'ShipBDt'.
    REPLACE ALL OCCURRENCES OF 'PORT' IN lv_string WITH 'Port'.
    REPLACE ALL OCCURRENCES OF 'REFCLM' IN lv_string WITH 'RefClm'.
    REPLACE ALL OCCURRENCES OF 'FORCUR' IN lv_string WITH 'ForCur'.
    REPLACE ALL OCCURRENCES OF 'CNTCODE' IN lv_string WITH 'CntCode'.
    REPLACE ALL OCCURRENCES OF 'TRANSID' IN lv_string WITH 'TransId'.
    REPLACE ALL OCCURRENCES OF 'TRANSNAME' IN lv_string WITH 'TransName'.
    REPLACE ALL OCCURRENCES OF 'TRANSMODE' IN lv_string WITH 'TransMode'.
    REPLACE ALL OCCURRENCES OF 'TRANSMode' IN lv_string WITH 'TransMode'.
    REPLACE ALL OCCURRENCES OF 'DISTANCE' IN lv_string WITH 'Distance'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCNO' IN lv_string WITH 'TransDocNo'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCNo' IN lv_string WITH 'TransDocNo'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCDT' IN lv_string WITH 'TransDocDt'.
    REPLACE ALL OCCURRENCES OF 'TRANSDOCDt' IN lv_string WITH 'TransDocDt'.
    REPLACE ALL OCCURRENCES OF 'VEHNO' IN lv_string WITH 'VehNo'.
    REPLACE ALL OCCURRENCES OF 'VEHNo' IN lv_string WITH 'VehNo'.
    REPLACE ALL OCCURRENCES OF 'VEHTYPE' IN lv_string WITH 'VehType'.
    REPLACE ALL OCCURRENCES OF 'VEHTypE' IN lv_string WITH 'VehType'.
    REPLACE ALL OCCURRENCES OF 'SLNo' IN lv_string WITH 'SlNo'.
    REPLACE ALL OCCURRENCES OF 'UnitPRICE' IN lv_string WITH 'UnitPrice'.
    REPLACE ALL OCCURRENCES OF 'TOTITEmVal' IN lv_string WITH 'TotItemVal'.
    REPLACE ALL OCCURRENCES OF 'TOTINVVal' IN lv_string WITH 'TotInvVal'.

    REPLACE ALL OCCURRENCES OF 'ASSVal' IN lv_string WITH 'AssVal'.
    REPLACE ALL OCCURRENCES OF 'CGSTVal' IN lv_string WITH 'CgstVal'.
    REPLACE ALL OCCURRENCES OF 'SGSTVal' IN lv_string WITH 'SgstVal'.
    REPLACE ALL OCCURRENCES OF 'IGSTVal' IN lv_string WITH 'IgstVal'.
    REPLACE ALL OCCURRENCES OF 'CESVal' IN lv_string WITH 'CESVal'.

    SHIFT lv_string LEFT DELETING LEADING '{"RESULT":'.
    REPLACE ALL OCCURRENCES OF '[{'  IN lv_string WITH '{'.
    REPLACE ALL OCCURRENCES OF '{'  IN lv_string WITH '{*'.
    REPLACE ALL OCCURRENCES OF ',"' IN lv_string WITH ',*"'.
    REPLACE ALL OCCURRENCES OF '}]' IN lv_string WITH '}'.
    REPLACE ALL OCCURRENCES OF '},' IN lv_string WITH '},*'.
    REPLACE ALL OCCURRENCES OF '}' IN lv_string WITH '*}'.
    REPLACE ALL OCCURRENCES OF '*}*}*}' IN lv_string WITH '*}*}'.
    REPLACE ALL OCCURRENCES OF 'ItemList":{' IN lv_string WITH 'ItemList":[{'.
    REPLACE ALL OCCURRENCES OF '},**"ATTRIBDtLS":{' IN lv_string WITH '},*"AttribDtls":[{'.
    REPLACE ALL OCCURRENCES OF '},**"ValDtls":' IN lv_string WITH '}]}],*"ValDtls":'.
    REPLACE ALL OCCURRENCES OF '"PRECDocDtls":' IN lv_string WITH '"PRECDocDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"ADDLDocDtls":' IN lv_string WITH '}]},*"ADDLDocDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"ExpDtls":' IN lv_string WITH '}],*"ExpDtls":'.
    REPLACE ALL OCCURRENCES OF '"CONTRDtLS":' IN lv_string WITH '"ContrDtls":['.
    REPLACE ALL OCCURRENCES OF '},**"BCHDtLS":' IN lv_string WITH ',*"BchDtls":'.
    REPLACE ALL OCCURRENCES OF '},**"DOCPRE":{' IN lv_string WITH ',*"DocPerdDtls":{'.
    REPLACE ALL OCCURRENCES OF '},**"ContrDtls":[{' IN lv_string WITH '}],*"ContrDtls":[{'.
    REPLACE ALL OCCURRENCES OF '**' IN lv_string WITH '*'.
    REPLACE ALL OCCURRENCES OF '"Distance": 0.0,' IN lv_string WITH '"Distance": 0,'.

    REPLACE ALL OCCURRENCES OF '""' IN lv_string WITH 'null'.
*    REPLACE ALL OCCURRENCES OF '111111' IN lv_string WITH 'null'.
    REPLACE ALL OCCURRENCES OF '*' IN lv_string WITH ''.
     REPLACE ALL OCCURRENCES OF '07ADtPM7389J1ZJ' IN lv_string WITH '07ADTPM7389J1ZJ'.
     REPLACE ALL OCCURRENCES OF '08ADtPC4299H1Z1' IN lv_string WITH '08ADTPC4299H1Z1'.
     REPLACE ALL OCCURRENCES OF '10ABIPh3151N1ZW' IN lv_string WITH '10ABIPH3151N1ZW'.
     REPLACE ALL OCCURRENCES OF '08ANoPR6820NIZ6' IN lv_string WITH '08ANOPR6820NIZ6'.
     REPLACE ALL OCCURRENCES OF '29AEEPh1574G1ZJ' IN lv_string WITH '29AEEPH1574G1ZJ' RESPECTING CASE.

   lv_string = '[{"transaction":' &&   lv_string && '}]'.

    result = lv_string.
    ENDMETHOD.
ENDCLASS.
