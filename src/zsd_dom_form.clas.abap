CLASS ZSD_DOM_FORM DEFINITION
   PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.

    INTERFACES if_oo_adt_classrun .


     CLASS-DATA : access_token TYPE string .
    CLASS-DATA : xml_file TYPE string .
   CLASS-DATA : template TYPE string ,
                copyno   TYPE N.
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



      read_posts
        importing VALUE(variable)  type char10
                 VALUE(variable1)  TYPE char10
                 VALUE(print)  TYPE STRING
                 VALUE(extraprint)  TYPE C OPTIONAL
                 VALUE(printtype) TYPE string OPTIONAL

        RETURNING VALUE(result12) TYPE string
        RAISING   cx_static_check .

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZSD_DOM_FORM IMPLEMENTATION.


  METHOD if_oo_adt_classrun~main.

*DATA(ts_ref1) =  xco_cp_time=>moment( iv_year   = '2024'
*                                      iv_month  = '06'
*                                      iv_day    = '11'
*                                      iv_hour   = '18'
*                                      iv_minute = '30'
*                                      iv_second = '00' ).
*
*
*DATA(ts19) = ts_ref1->add( iv_hour = 5 iv_minute = 30 iv_second = 0
*                          )->as( xco_cp_time=>format->iso_8601_extended
*                          )->value.



 ENDMETHOD.


  METHOD read_posts.

      DATA TEMPLATE TYPE STRING.
      DATA : zdate(10) TYPE c .
      DATA : ztime    TYPE sy-timlo .

    zdate = |{ sy-datum+6(2) }/{ sy-datum+4(2) }/{ sy-datum+0(4) }| .
    ztime = sy-timlo .


   SELECT * FROM  yeinvoice_cdss WITH PRIVILEGED ACCESS  as a  WHERE billingdocument = @variable  INTO TABLE @DATA(IT).

DELETE it WHERE BillingQuantity = 0 ."and NetAmount = 0.

sort it  by BillingDocument BillingDocumentItem.
DELETE ADJACENT DUPLICATES FROM it COMPARING BillingDocument BillingDocumentItem.

   READ TABLE IT INTO DATA(HEADWA) INDEX 1.

   if HEADWA-DistributionChannel = '17'.
   TEMPLATE = 'ZSD_IRN_EXPORT'.
   ELSE.
   if HEADWA-BillingDocumentType = 'F8' or HEADWA-BillingDocumentType = 'JSN'.
   TEMPLATE = 'Delivery_Challan_STO_Print'.
   ELSE.
   TEMPLATE = 'ZSD_IRN'.
   ENDIF.
   ENDIF.

   SELECT SINGLE AccountingExchangeRate FROM I_BillingDocument   WITH PRIVILEGED ACCESS
     WHERE BillingDocument = @headwa-BillingDocument INTO @DATA(AccountingExchangeRate).

      SELECT SINGLE validupto FROM yj1ig_ewaybill   WITH PRIVILEGED ACCESS
     WHERE docno = @headwa-BillingDocument AND status = 'A'  INTO @DATA(validupto).

*DATA: lo_tstmp   TYPE TZNTSTMPS,
*      INV_TIME  TYPE  sy-uzeit.

 SELECT SINGLE * FROM  zship_add WHERE Plant = @headwa-Plant INTO @DATA(plant_add)  .

SELECT SINGLE  RegionName from I_RegionText WHERE Region =   @plant_add-Region and Language = 'E' and Country = 'IN' INTO @data(state).
SELECT SINGLE  CountryName from I_CountryText WHERE Country =   @plant_add-Country and Language = 'E' INTO @data(con_name).

DATA lv_xml TYPE STRING.

data add1 TYPE string .
data com_name TYPE string .
data comcode TYPE string .
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
data header TYPE string.
data header2 TYPE string.


 DATA(remark)  = zbill_head_text=>bill_text(  billingdocument =  variable  billingdocumenttextid = 'TX06' )  .
 "DATA(driver) = zbill_head_text=>bill_text(  billingdocument =  variable  billingdocumenttextid = 'ZDRD' )  .

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""plant add"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
*add1 = plant_add-OrganizationName1.
comcode = HEADWA-CompanyCode.
com_name = plant_add-AddresseeFullName.
add1 = | { plant_add-StreetName } { plant_add-StreetPrefixName1 }  { plant_add-StreetPrefixName2 } | .
add2 = | { plant_add-StreetSuffixName1 }  { plant_add-StreetSuffixName2 } | .
add3 = | { plant_add-CityName }  { plant_add-DistrictName }  { plant_add-PostalCode } { state } ({ plant_add-Region }) { con_name } | .
cin = ''.

selECT siNGLE * frOM zplant_data whERE plant = @headwa-Plant into @data(plantadd).

gst = plantadd-gstin .
pan = plantadd-pan.
cin = plantadd-cin_no .


*if headwa-Plant = '1100' or headwa-Plant = '1110' or headwa-Plant = '1120' or headwa-Plant = '1130'
*or headwa-Plant = '1140' or headwa-Plant = '1210' or headwa-Plant = '1220' or headwa-Plant = '1230' or headwa-Plant = '1500'.
*gst = '08AAGCV0492E1ZB'.
*pan = 'AAGCV0492E'.
*cin = 'U24100RJ2017PLC058778'.
*ELSEIF headwa-Plant = '1000'.
*gst = '23AAGCV0492E1ZJ'.
*pan = 'AAGCV0492E'.
*cin = 'U24100RJ2017PLC058778'.
*ELSEIF headwa-Plant = '2100'.
*gst = '08AEQPG8635R3ZF'.
*pan = 'AEQPG8635R'.
*cin = ''.
*ELSEIF headwa-Plant = '3100'.
*gst = '23AAGCV4391L1ZY'.
*cin = 'U46909MP2018PTC045881'.
*pan = 'AAGCV4391L'.
*ELSEIF headwa-Plant = '3200'.
*gst = '33AAGCV4391L1ZX'.
*pan = 'AAGCV4391L'.
*cin = 'U46909MP2018PTC045881'.
*ELSEIF headwa-Plant = '3300'.
*gst = '29AAGCV4391L1ZM'.
*pan = 'AAGCV4391L'.
*cin = 'U46909MP2018PTC045881'.
*ENDIF.


"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""


        SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' AND K~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on B~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'WE'  AND c~billingdocument = @headwa-BillingDocument
        "where C~BillingDocument = @variable
        INTO @DATA(shipto1)  .

SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN i_customer AS b ON ( a~customer = b~customer )
        INNER JOIN zshipto WITH PRIVILEGED ACCESS AS c ON ( c~billingdocument = a~billingdocument  AND c~customer = a~customer )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' AND K~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on B~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'RE'  AND c~billingdocument =  @headwa-BillingDocument
        "where C~BillingDocument = @variable
        INTO @DATA(billto)  .

SHIFT headwa-BillingDocument LEFT DELETING LEADING '0'.
SHIFT headwa-delivery_number LEFT DELETING LEADING '0'.

DATA: lt_tax TYPE STANDARD TABLE OF I_BillingDocumentItem-TaxCode,
      lv_all_a0 TYPE abap_bool.

if     headwa-BillingDocumentType = 'F2' OR headwa-BillingDocumentType = 'JSTO'.
header = 'TAX INVOICE '.
ELSEIF headwa-BillingDocumentType = 'F8'.
header = 'DELIVERY CHALLAN'.
ELSEIF headwa-BillingDocumentType = 'L2'.
header = 'DEBIT NOTE'.
ELSEIF headwa-BillingDocumentType = 'G2'.
header = 'CREDIT NOTE'.
ELSEIF headwa-BillingDocumentType = 'CBRE'.
header = 'RETURN CREDIT NOTE'.
ELSEIF headwa-BillingDocumentType = 'JSN'.
header = 'JOBWORK CHALLAN'.
ENDIF.

IF ( headwa-BillingDocumentType = 'F2' OR headwa-BillingDocumentType = 'JSTO' ) and headwa-DistributionChannel <> '17' .
 SELECT TaxCode FROM I_BillingDocumentItem WITH PRIVILEGED ACCESS
 WHERE BillingDocument = @headwa-BillingDocument GROUP BY TaxCode INTO TABLE @lt_tax.

  lv_all_a0 = abap_true.

  LOOP AT lt_tax INTO DATA(ls_tax).
    IF ls_tax <> 'A0'.
      lv_all_a0 = abap_false.
      EXIT.
    ENDIF.
  ENDLOOP.

  IF lv_all_a0 = abap_true.
    header2 = 'BILL OF SUPPLY'.
  ELSE.
    header2 = header.
  ENDIF.

ELSE.

  header2 = header.

ENDIF.


 SELECT single IncotermsClassification, CustomerPaymentTerms, PurchaseOrderByCustomer ,CustomerPurchaseOrderDate,SalesDocumentDate FROM I_SalesDocument  where SalesDocument = @headwa-sddocu INTO   @DATA(sd).

 SELECT single ShippingTypeName  FROM I_SHIPPINGTYPETEXT  WHERE ShippingType =  @headwa-ShippingType and Language = 'E' INTO   @DATA(ShippingType).

 SELECT SINGLE Deliveryterms , Finaldestination , Incotermslocation ,PaymentTerms ,portofdischarge,portofloading
 FROM zpregen_exi WHERE docno = @variable AND Doctype = 'PO'  INTO @DATA(gendata1) .

 SELECT SINGLE lcdate ,lcno  FROM yexim_calculat WHERE docno = @variable AND Doctype = 'PO'  INTO @DATA(gendata2) .

 SELECT SINGLE firmregistrationno ,eximcode FROM yconsignedata_pr WHERE docno = @variable AND Doctype = 'PO'  INTO @DATA(gendata3) .
 SELECT SINGLE CreationDate FROM I_PURCHASEORDERAPI01 WHERE PurchaseOrder = @headwa-sddocu   INTO @DATA(podate) .

data invoice_name TYPE string.
data invdated TYPE string.

if headwa-BillingDocumentType = 'F8' or headwa-BillingDocumentType = 'JSN'.
invoice_name = 'Challan No.' .
invdated = 'Challan Date' .
 SELECT single IncotermsClassificationName  FROM I_IncotermsClassificationText  WHERE IncotermsClassification =  @HEADWA-IncotermsClassification and Language = 'E' INTO   @DATA(inco).
 SELECT single PaymentTermsName  FROM I_PaymentTermsText  WHERE PaymentTerms =  @HEADWA-CustomerPaymentTerms and Language = 'E' INTO   @DATA(pay_terms).

else.
invoice_name = 'Invoice No.' .
invdated = 'Invoice Date' .

 SELECT single PaymentTermsName  FROM I_PaymentTermsText  WHERE PaymentTerms =  @sd-CustomerPaymentTerms and Language = 'E' INTO   @pay_terms.
 SELECT single IncotermsClassificationName  FROM I_IncotermsClassificationText  WHERE IncotermsClassification =  @sd-IncotermsClassification and Language = 'E' INTO   @inco.

endif.



DATA delivery_note TYPE string.
DATA delivery_note2 TYPE string.

SELECT ReferenceSDDocument from I_BillingDocumentItem WITH PRIVILEGED ACCESS WHERE BillingDocument = @headwa-BillingDocument
 GROUP BY ReferenceSDDocument into TABLE @data(del).

sort del by ReferenceSDDocument.

loop at del into data(wdel).
data n type string.
n = n + 1.
delivery_note = wdel-ReferenceSDDocument.
if n = 1.
CONCATENATE delivery_note2 delivery_note INTO delivery_note2 SEPARATED BY ' '.
else.
CONCATENATE delivery_note2 delivery_note INTO delivery_note2 SEPARATED BY ','.
ENDIF.

ENDLOOP.

if headwa-TRANSPORTERNAME is INITIAL .
headwa-TRANSPORTERNAME = headwa-trans.
ENDIF.


SELECT SINGLE A~PurchaseOrder , B~CreationDate from I_MATERIALDOCUMENTITEM_2 WITH PRIVILEGED ACCESS AS A
LEFT OUTER JOIN I_PurchaseOrderAPI01 WITH PRIVILEGED ACCESS AS B ON ( A~PurchaseOrder = B~PurchaseOrder )
WHERE MATERIALDOCUMENT = @wdel-ReferenceSDDocument
  into  @data(PurchaseOrder_JSN).


if headwa-BillingDocumentType = 'JSN'.
ShippingType = 'Road'.
sd-PurchaseOrderByCustomer = PurchaseOrder_JSN-PurchaseOrder.
sd-CustomerPurchaseOrderDate = PurchaseOrder_JSN-CreationDate.

ENDIF.



**********************************************************************

DATA: lo_tstmp   TYPE TZNTSTMPS,
      INV_TIME  TYPE  sy-uzeit,
      INV_date  TYPE  sy-datum.
if HEADWA-BillingDocumentDate <> 00000000 .

   DATA GG TYPE STRING.
      convert date HEADWA-BillingDocumentDate  time HEADWA-CreationTime
      into time stamp lo_tstmp time zone 'UTC'.    " HHH

data(INV)   = cl_abap_tstmp=>add( tstmp = lo_tstmp
                                secs  =  19800 ).

CONVERT TIME STAMP INV TIME ZONE 'UTC' INTO DATE inv_date TIME inv_time.

ENDIF.

**********************************************************************
  lv_xml =

|<form1>|  &&
|<com_name>{ com_name }</com_name>| &&
|<comcode>{ comcode }</comcode>| &&
|<add1>{ add1 }</add1>|  &&
|<add2>{ add2 } { add3 }</add2>|  &&
|<add3></add3>|  &&
|<add4></add4>|  &&
|<add5></add5>|  &&
|<QRCodeBarcode1>{ headwa-SignedQrcode }</QRCodeBarcode1>|  &&
|<pan>PAN NO.- { pan }</pan>| &&
|<cin>CIN NO.- { cin }</cin>| &&
|<gst>GSTIN:- { gst }</gst>| &&
|<Header>{ header2 }</Header>|  &&
|<ship1>{ shipto1-C-AddresseeFullName }</ship1>|  &&
|<ship2>{ shipto1-C-Street } { shipto1-C-StreetName } { shipto1-C-StreetPrefixName1 } { shipto1-C-StreetPrefixName2 } { shipto1-C-StreetSuffixName1 } { shipto1-C-StreetSuffixName2 }  { shipto1-b-PostalCode }</ship2>|  &&
|<ship3>{ shipto1-b-CityName } { shipto1-b-DistrictName } { shipto1-k-RegionName },{ shipto1-m-CountryName } </ship3>|  &&
|<ship4>GSTIN- { shipto1-b-TaxNumber3 }</ship4>|  &&
|<ship5>{ shipto1-c-TelephoneNumber1 }</ship5>|  &&
|<bill1>{ billto-C-AddresseeFullName }</bill1>|  &&
|<bill2>{ billto-C-Street } { billto-C-StreetName } { billto-C-StreetPrefixName1 } { billto-C-StreetPrefixName2 } { billto-C-StreetSuffixName1 } { billto-C-StreetSuffixName2 } { billto-b-PostalCode }</bill2>|  &&
|<bill3>{ billto-b-CityName } { billto-b-DistrictName } { billto-k-RegionName },{ billto-m-CountryName }</bill3>|  &&
|<bill4>GSTIN- { billto-b-TaxNumber3 }</bill4>|  &&
|<bill5>{ billto-c-TelephoneNumber1 }</bill5>|  &&
|<FirmRegNo>{ gendata3-firmregistrationno }</FirmRegNo>|  &&
|<EximCode>{ gendata3-eximcode  }</EximCode>|  &&
|<irn>{ headwa-Irn }</irn>| &&
|<eway>{ headwa-Ebillno }</eway>| &&
|<validto>{ headwa-validupto+6(2) }/{ headwa-validupto+4(2) }/{ headwa-validupto+0(4) }</validto>| &&
|<distance>{ cond #( when headwa-Distance2 <> 0 then headwa-Distance2 else headwa-Distance ) }</distance>| &&
|<transmode>{ ShippingType }</transmode>| &&
|<ackno>{ headwa-AckNo }</ackno>| &&
|<ackdate>{ headwa-AckDate }</ackdate>| &&

|<challan_no>{ headwa-BillingDocument  }</challan_no>| &&
|<invoice_no>{ headwa-BillingDocument }</invoice_no>|  .

if headwa-BillingDocumentType <> 'L2' and headwa-BillingDocumentType <> 'G2'.
lv_xml = lv_xml &&
|<delivery_note>{ delivery_note2 }</delivery_note>|  .
endif .

   lv_xml = lv_xml &&

|<ref_no>{ sd-PurchaseOrderByCustomer }</ref_no>|  &&
|<ref_date>{ sd-CustomerPurchaseOrderDate+6(2) }-{ sd-CustomerPurchaseOrderDate+4(2) }-{ sd-CustomerPurchaseOrderDate+0(4) }</ref_date>| &&
|<ref_date></ref_date>| &&
|<order_no>{ headwa-sddocu }</order_no>|  &&
|<lrno>{ headwa-yy1_lrno1_bdh }</lrno>| &&
|<Lrdate>{ headwa-YY1_LRDate_BDH+6(2) }-{ headwa-YY1_LRDate_BDH+4(2) }-{ headwa-YY1_LRDate_BDH+0(4) }</Lrdate>| &&
|<dispatch_doc></dispatch_doc>|  &&
|<dispatch_through></dispatch_through>|  &&
|<Bill_of_loding></Bill_of_loding>| &&
|<invoice_name>{ invoice_name }</invoice_name>| &&
|<invoice_name2>{ invoice_name }          :</invoice_name2>| &&
|<invdated>{ invdated }</invdated>|  &&
|<invdated2>{ invdated }          :</invdated2>|  .

if headwa-BillingDocumentType = 'F8' or headwa-BillingDocumentType = 'JSN'.
lv_xml = lv_xml &&
|<dated2>{ inv_date+6(2) }-{ inv_date+4(2) }-{ inv_date+0(4) }</dated2>| &&
|<date>{ podate+6(2) }-{ podate+4(2) }-{ podate+0(4) }</date>|  &&
|<dated>{  inv_date+6(2) }-{  inv_date+4(2) }-{  inv_date+0(4) }</dated>|  .


*SSS
else.
lv_xml = lv_xml &&
|<dated>{  inv_date+6(2) }-{  inv_date+4(2) }-{  inv_date+0(4) }</dated>|  .
ENDIF.

lv_xml = lv_xml &&
|<other_ref></other_ref>|  &&
|<date>{ sd-SalesDocumentDate+6(2) }-{ sd-SalesDocumentDate+4(2) }-{ sd-SalesDocumentDate+0(4) }</date>|  &&
|<delivery_note></delivery_note>|  .

if HEADWA-DistributionChannel = '17'.
lv_xml = lv_xml &&
|<LCNo>{ gendata2-lcno }</LCNo>|  &&
|<LCDate>{ gendata2-lcdate+8(2) }-{ gendata2-lcdate+5(2) }-{ gendata2-lcdate+0(4) }</LCDate>|  &&
|<destination>{ gendata1-Finaldestination }</destination>|  &&
|<terms_of_delivery>{ gendata1-Deliveryterms } { gendata1-Incotermslocation }</terms_of_delivery>|  &&
|<paymentMode>{ gendata1-PaymentTerms }</paymentMode>|  .
else.
 lv_xml = lv_xml &&
|<destination>{ shipto1-b-CityName }</destination>|  &&
|<terms_of_delivery>{ inco }</terms_of_delivery>|  &&
|<paymentMode>{ pay_terms }</paymentMode>|  .
ENDIF.

lv_xml = lv_xml &&
|<moter_vehicle>{ headwa-YY1_VehicleNo_BDH }</moter_vehicle>|  &&
|<drivername>{ headwa-YY1_DriverDetails_BDH }</drivername>| &&
|<portofdicharge>{ gendata1-portofdischarge  }</portofdicharge>| &&
|<portofloading>{ gendata1-portofloading }</portofloading>| &&
|<transporter>{ headwa-TRANSPORTERNAME }</transporter>|.

DATA(it_fin) = it[].

 LOOP AT it INTO DATA(WA_ITEM)  .
 data gstrate TYPE p DECIMALS 2.
 data cgstamt TYPE p DECIMALS 2.
 data sgstamt TYPE p DECIMALS 2.
 data igstamt TYPE p DECIMALS 2.
 data TotaLAmt TYPE p     DECIMALS 2.
 data qty TYPE p DECIMALS 2.

 sno = sno + 1.

 if WA_ITEM-igstrate is not INITIAL or WA_ITEM-igstrate <> '' or WA_ITEM-igstrate <> '0'.
 gstrate = WA_ITEM-igstrate .
 ENDIF.

 if WA_ITEM-cgstrate is not INITIAL or WA_ITEM-cgstrate <> '' or WA_ITEM-cgstrate <> '0'.
 gstrate = WA_ITEM-cgstrate * 2.
 ENDIF.

SELECT SINGLE UnitOfMeasureLongName from I_UnitOfMeasureText WHERE UnitOfMeasure = @wa_item-BillingQuantityUnit and Language = 'E' INTO @data(uom).
if wa_item-BillingQuantityUnit = 'KG' or wa_item-BillingQuantityUnit = 'NO'.
uom  = wa_item-BillingQuantityUnit.
ENDIF.

*selECT siNGLE * frOM ZI_BillingDocumentItemPrcgElmn WHERE BillingDocument = @WA_ITEM-BillingDocument
*AND BillingDocumentItem = @WA_ITEM-BillingDocumentItem AND ConditionType = 'D100' into @data(D100).

 SELECT SINGLE FROM @it_fin AS a FIELDS
          SUM( Basic_Amount ) AS Basic_Amount,
          SUM( BillingQuantity ) AS BillingQuantity,
          SUM( IGST )     AS IGST,
          SUM( CGST )     AS CGST,
          SUM( SGST ) AS SGST,
          sum( dis_amt ) as dis_amt,
          sum( she_amt ) as she_amt,
          sum( zdqt_amt ) as zdqt_amt,
          sum( PCIP_AMT1 ) as PCIP_AMT1
          WHERE MaterialDescription = @wa_item-MaterialDescription
          AND BillingDocumentItem  = @WA_ITEM-BillingDocumentItem
          AND basicrate = @wa_item-basicrate
          INTO  @DATA(sumamtqty).

TotaLAmt =  ( sumamtqty-Basic_Amount + sumamtqty-dis_amt + sumamtqty-she_amt + sumamtqty-zdqt_amt  + sumamtqty-PCIP_AMT1 ) * AccountingExchangeRate.

*IF D100-ConditionType = 'D100'.
*TotaLAmt = '0.00'.
*ENDIF.

data gstcheck type string.
data gstcheck2 type string.
data gstr type string.
data taxable TYPE p DECIMALS 2.
data uintprice TYPE p DECIMALS 2.
DATA RATE TYPE P DECIMALS 2.
DATA totamt TYPE P DECIMALS 2.
DATA totqty TYPE P DECIMALS 2.
DATA disc TYPE P DECIMALS 2.
DATA scheme TYPE P DECIMALS 2.
DATA qtydis TYPE P DECIMALS 2.

scheme = wa_item-sherate * -1.
disc = wa_item-disrate * -1.
qtydis = wa_item-zdqtrate * -1.
IF headwa-BillingDocumentType = 'F8' or headwa-BillingDocumentType = 'JSN' or headwa-BillingDocumentType = 'JSTO'.
IF headwa-unit = 'CS' or headwa-unit = 'BAG'.
RATE = WA_ITEM-PCIP_RATE * headwa-QuantityNumerator.
ELSE.
RATE = WA_ITEM-PCIP_RATE.
WA_ITEM-basicrate = WA_ITEM-PCIP_RATE.
ENDIF.
ELSE.
RATE = WA_ITEM-basicrate * AccountingExchangeRate.
ENDIF.
taxable   = wa_item-Basic_Amount.
uintprice = WA_ITEM-basicrate  .
******************************

data(LONGTEXT) = zsaleorder_itemtext=>Sales_Order_ItemText_DATA(  salesorder =  WA_ITEM-sddocu salesorderitem = WA_ITEM-sddocuitem  salesorderitemtextid = '0001' )  .
data text(1000) TYPE c.

IF LONGTEXT <> ''.
text = LONGTEXT.
else.
text = WA_ITEM-MaterialDescription.
ENDif.

gstcheck2 = gstcheck.

gstcheck = gstrate.

if sno <> 1.
if gstcheck2 <> gstcheck.
gstr = 0.
Else.
gstr = 1.
ENDIF.
ENDIF.

    lv_xml = lv_xml &&

|<Row2>| &&
|<srno>{ sno }</srno>| &&
|<description>({ wa_item-Material }) { text }</description>| &&
|<hsn>{ wa_item-Hsncode }</hsn>| &&
|<quantity>{ wa_item-BillingQuantity }</quantity>| &&
|<rate>{ RATE }</rate>| &&
|<per>{ uom }</per>| &&
|<disc>{ disc }</disc>| &&
|<scheme>{ scheme }</scheme>| &&
|<qtydis>{ qtydis }</qtydis>| &&
|<gstrate>{ gstrate }</gstrate>| &&
|<amt>{ TotaLAmt }</amt>| &&
|</Row2>| .

  cgstamt = cgstamt + sumamtqty-cgst.
  sgstamt = sgstamt + sumamtqty-sgst.
  igstamt = igstamt + sumamtqty-igst.
totqty = totqty + wa_item-BillingQuantity.
totamt = totamt + TotaLAmt.
clear : gstrate,WA_ITEM-sgst,WA_ITEM-cgst,WA_ITEM-igst, qty,sumamtqty,taxable,uintprice,RATE,LONGTEXT .

 ENDLOOP.

   lv_xml = lv_xml &&

|<totrow>| &&
|<totqty>{ totqty }</totqty>| &&
|<totamt>{ totamt }</totamt>| &&
|</totrow>| .

  SELECT   SUM( Basic_Amount ) AS Basic_Amount,
           sum( PCIP_AMT1 ) as PCIP_AMT1,
          SUM( IGST )     AS IGST,
          SUM( CGST )     AS CGST,
          SUM( SGST ) AS SGST,
          sum( dis_amt ) as dis_amt,
          sum( she_amt ) as she_amt,
          sum( zdqt_amt ) as zdqt_amt,
          SUM( zcdi_amt ) AS zcdi_amt,
          SUM( zcde_amt ) AS zcde_amt,
          SUM( zdih_amt ) AS zdih_amt,
          SUM( zdof_amt ) AS zdof_amt,
          SUM( zfo1_amt ) AS zfo1_amt,
          cgstrate ,
          sgstrate ,
          IGSTRATE ,
          Hsncode AS Hsncode FROM yeinvoice_cdss WHERE  billingdocument = @variable
          GROUP BY Hsncode,cgstrate,sgstrate,IGSTRATE
          INTO TABLE @DATA(HSN).

LOOP AT HSN INTO DATA(WAHSN).

data cgst_rate2 TYPE p DECIMALS 2.
data sgst_rate2 TYPE p DECIMALS 2.
data igst_rate2 TYPE p DECIMALS 2.
data tottaxamt TYPE p DECIMALS 2.
data tot_tax TYPE p DECIMALS 2.
data tot_cgst TYPE p DECIMALS 2.
data tot_sgst TYPE p DECIMALS 2.
data tot_igst TYPE p DECIMALS 2.
data tot_amt TYPE p DECIMALS 2.
data taxvalue TYPE p DECIMALS 2.

taxvalue = ( WAHSN-Basic_Amount + WAHSN-dis_amt + WAHSN-she_amt + WAHSN-zdqt_amt + WAHSN-zfo1_amt  + WAHSN-zcdi_amt + WAHSN-zcde_amt + WAHSN-PCIP_AMT1 ) * AccountingExchangeRate..
tot_tax = tot_tax + taxvalue.
sgst_rate2 = wahsn-sgstrate.
cgst_rate2 = wahsn-cgstrate.
igst_rate2 = wahsn-igstrate.
tottaxamt =  ( wahsn-igst + wahsn-cgst + wahsn-sgst ) * AccountingExchangeRate.

tot_cgst = tot_cgst + wahsn-cgst.
tot_sgst = tot_sgst + wahsn-sgst.
tot_igst = ( tot_igst + wahsn-igst ) * AccountingExchangeRate.
tot_amt = ( tot_amt + tottaxamt ) .

lv_xml = lv_xml &&
|<hsnrow>| &&
|<hsn>{ WAHSN-hsncode }</hsn>| &&
|<taxvalue>{ taxvalue }</taxvalue>| &&
|<cgstrate>{ cgst_rate2 }</cgstrate>| &&
|<cgstamt>{ wahsn-cgst }</cgstamt>| &&
|<sgatrate>{ sgst_rate2 }</sgatrate>| &&
|<sgstamt>{ wahsn-sgst }</sgstamt>| &&
|      <igstrate>{ igst_rate2 }</igstrate>| &&
|      <igstamt>{ wahsn-igst * AccountingExchangeRate }</igstamt>| &&
|<taxamount>{ tottaxamt }</taxamount>| &&
|</hsnrow>| .
clear : wahsn,cgst_rate2,sgst_rate2,igst_rate2,tottaxamt,taxvalue.
ENDLOOP.

lv_xml = lv_xml &&

|<hsntotrow>| &&
|<Cell2></Cell2>| &&
|<tot_tax>{ tot_tax }</tot_tax>| &&
|<Cell4></Cell4>| &&
|<tot_cgst>{ tot_cgst }</tot_cgst>| &&
|<Cell6></Cell6>| &&
|<tot_sgst>{ tot_sgst }</tot_sgst>| &&
|<tot_igst>{ tot_igst }</tot_igst>| &&
|<totaltaxamount>{ tot_amt }</totaltaxamount>| &&
|<tot_amt>{ tot_amt }</tot_amt>| &&
|</hsntotrow>| .


  SELECT SINGLE FROM @it_fin AS a FIELDS
          SUM( Basic_Amount ) AS Basic_Amount,
          SUM( IGST )     AS IGST,
          SUM( CGST )     AS CGST,
          SUM( SGST )     AS SGST,
          SUM( zcdi_amt ) AS zcdi_amt,
          SUM( zcde_amt ) AS zcde_amt,
          SUM( zdih_amt ) AS zdih_amt,
          SUM( zdof_amt ) AS zdof_amt,
          SUM( zfo1_amt ) AS zfo1_amt,
          sum( ztcs_amt ) AS ztcs_amt
          WHERE BillingDocument = @variable
          INTO  @DATA(tax).


data tot_amt22 TYPE p DECIMALS 2.
data cashamt TYPE p DECIMALS 2.
data caseamt TYPE p DECIMALS 2.
data cashrate TYPE p DECIMALS 2.
data festamt TYPE p DECIMALS 2.
data rof2 TYPE p DECIMALS 2.
data igst_rate TYPE string.
data cgst_rate TYPE string.
data sgst_rate TYPE string.


  SELECT SINGLE FROM @it_fin AS a FIELDS
         cast(  cgstrate as dec( 13 ,2 ) ) as cgstrate ,
         cast(  sgstrate as dec( 13 ,2 ) ) as sgstrate,
         cast(  igstrate as dec( 13 ,2 ) ) as igstrate,
         cast(  zcdirate as dec( 13 ,2 ) ) as zcdirate,
         cast(  zdihrate as dec( 13 ,2 ) ) as zdihrate,
         cast(  zfo1rate as dec( 13 ,2 ) ) as zfo1rate,
         cast(  ztcsrate as dec( 13 ,2 ) ) as ztcsrate
         WHERE BillingDocument = @variable
         INTO  @data(rate22).

if HEADWA-DistributionChannel = '17'.
cashamt = TAX-zcde_amt * -1 .
ELSE.
cashamt = TAX-zcdi_amt * -1 .
ENDIF.

cashrate = rate22-zcdirate * -1 .
festamt = tax-zfo1_amt * -1.

if gstr = 0.
cgst_rate = ''.
sgst_rate = ''.
igst_rate = ''.
else.
cgst_rate = |{ rate22-cgstrate }%|.
sgst_rate = |{ rate22-sgstrate }%|.
igst_rate = |{ rate22-igstrate }%|.
ENDIF.

data cancel TYPE string.
data draft TYPE string.

if     headwa-BillingDocumentType = 'F8'.
if  HEADWA-BillingDocumentIsCancelled = '' and HEADWA-Ebillno = ''.
draft = 'X'.
ENDIF.
ELSEIF  headwa-BillingDocumentType = 'JSN'.
draft = ''.
ELSE.
if HEADWA-Irn = '' and  HEADWA-BillingDocumentIsCancelled = '' and HEADWA-Ebillno = ''.
draft = 'X'.
ENDIF.
ENDIF.

if HEADWA-BillingDocumentIsCancelled = 'X' OR headwa-CancelledBillingDocument <> ''.
cancel = 'X'.
ENDIF.

tot_amt22 = totamt + TAX-zcdi_amt + TAX-zcde_amt + tax-zfo1_amt + TAX-zdih_amt + tax-cgst + tax-sgst + ( tax-igst * AccountingExchangeRate ) + tax-zdof_amt + tax-ztcs_amt .

DATA GROSAMT TYPE P DECIMALS 2 .

GROSAMT = ( HEADWA-docuhead_netamt + HEADWA-docuhead_total_tax ) * AccountingExchangeRate.


if HEADWA-DistributionChannel <> '17'.
lv_xml = lv_xml &&
|   <cashrate>{ cashrate }%</cashrate>| .
 ENDIF.

 SELECT  SINGLE PartnerFunction  FROM I_BillingDocumentPartner  where BillingDocument = @headwa-BillingDocument AND PartnerFunction = 'ZD' INTO  @DATA(i_BillingPart).
   SELECT SINGLE * FROM  i_billingdocumentpartner WITH PRIVILEGED ACCESS AS a  INNER JOIN I_Supplier AS b ON ( a~ReferenceBusinessPartner = b~Supplier )
        INNER JOIN ZI_Address_2 WITH PRIVILEGED ACCESS AS c ON ( c~AddressID = B~AddressID )
        LEFT OUTER JOIN I_RegionText as k on c~Region = k~Region and k~Language = 'E' and k~Country = 'IN'
        LEFT OUTER JOIN I_CountryText as m on k~Country = m~Country and m~Language = 'E'
        WHERE a~partnerfunction = 'ZD'  AND A~billingdocument = @headwa-BillingDocument
        INTO @DATA(disp)  .
lv_xml = lv_xml &&
|   <cashamt>{ cashamt }</cashamt>| &&
|   <festrate></festrate>| &&
|   <festamt>{ festamt  }</festamt>| &&
|   <Per_Rate>{ rate22-zdihrate } %</Per_Rate>| &&
|   <per_amt>{ TAX-zdih_amt }</per_amt>| &&
|   <tcs_Rate>{ rate22-ztcsrate } %</tcs_Rate>| &&
|   <tcs_amt>{ TAX-ztcs_amt }</tcs_amt>| &&
"|   <round_off_rate>{ tax-zdofrate } %</round_off_rate>| &&
|   <round_off_amt>{ tax-zdof_amt }</round_off_amt>| &&
|   <freight_rate></freight_rate>| &&
|   <freight_amt></freight_amt>| &&
|    <Taxable>{ totamt + TAX-zcdi_amt + TAX-zcde_amt + tax-zfo1_amt + TAX-zdih_amt }</Taxable>| &&
|   <cgst_rate>{ cgst_rate }</cgst_rate>| &&
|   <gst_amt>{ tot_amt }</gst_amt>| &&
|   <cgst_amt>{ cgstamt }</cgst_amt>| &&
|   <sgst_rate>{ sgst_rate }</sgst_rate>| &&
|   <sgst_amt>{ sgstamt }</sgst_amt>| &&
|   <igst_rate>{ igst_rate }</igst_rate>| &&
|   <igst_amt>{ igstamt * AccountingExchangeRate }</igst_amt>| &&
"|   <tot_amt>{ tot_amt22 }</tot_amt>| &&
|   <tot_amt>{ GROSAMT }</tot_amt>| &&
|   <remark>{ remark }</remark>| &&
|   <partner>{ i_BillingPart }</partner>| &&
|<dis1>{ disp-c-AddresseeFullName }</dis1>|  &&
|<dis2>{ disp-C-Street } { disp-C-StreetName } { disp-C-StreetPrefixName1 } { disp-C-StreetPrefixName2 } { disp-C-StreetSuffixName1 } { disp-C-StreetSuffixName2 } { disp-b-PostalCode }</dis2>|  &&
|<dis3> { disp-b-CityName } { disp-b-DistrictName } { disp-k-RegionName },{ disp-m-CountryName }</dis3>|  &&
|<dis4>GSTIN- { disp-b-TaxNumber3 }</dis4>|  &&
|   <com_pan></com_pan>| &&
|   <printtype>{ print }</printtype>| &&
|<cancel>{ cancel }</cancel>| &&
|<draft>{ draft }</draft>| &&
|<com>For { com_name }</com>| &&
|<preparedby>{ cl_abap_context_info=>get_user_formatted_name( iv_buser = HEADWA-CreatedByUser ) }</preparedby>| &&
|<BILLINGTYPE>{ headwa-BillingDocumentType }</BILLINGTYPE>| &&
|</form1>| .

  REPLACE ALL OCCURRENCES OF '&' IN lv_xml WITH 'and'.
  CALL METHOD zadobe_print=>adobe( EXPORTING
     xml  = lv_xml
     form_name = TEMPLATE
     RECEIVING
      result   = result12 ).

  ENDMETHOD.
ENDCLASS.
