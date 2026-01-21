CLASS zsd_generate_ewaybill DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
   INTERFACES if_oo_adt_classrun .

    CLASS-DATA : access_token TYPE string .
    CLASS-DATA : final_message_ewaybill TYPE string .

  CLASS-METHODS :
   generate_ewaybill
        IMPORTING billingdocument TYPE CHAR10 OPTIONAL
                  companycode     TYPE CHAR4 OPTIONAL
                  distance        TYPE string OPTIONAL
                  transpoter_name TYPE string OPTIONAL
                  vehiclenumber   TYPE string  OPTIONAL
                  transportid     TYPE string  OPTIONAL
                  transportdoc    TYPE string OPTIONAL
                  eway_generate   TYPE string OPTIONAL
        RETURNING VALUE(status)   TYPE string  .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZSD_GENERATE_EWAYBILL IMPLEMENTATION.


 METHOD generate_ewaybill.

   DATA GV TYPE STRING .
    DATA GV1 TYPE STRING .
    DATA GV2 TYPE STRING .
    DATA GV3 TYPE STRING .


    SELECT SINGLE * FROM  I_BillingDocumentBasic WHERE BillingDocument = @billingdocument  INTO @DATA(INVOICEDATE)   .
    SELECT SINGLE * FROM  I_BillingDocumentPartner WHERE BillingDocument = @billingdocument
                          AND PartnerFunction =  'ZT' INTO @DATA(TRANSPORTPARTRNER) .
                      SELECT SINGLE * FROM I_SUPPLIER WHERE Supplier = @TRANSPORTPARTRNER-SUPPLIER  INTO @DATA(TRANSPORTPARTRNER1)   .
    SELECT SINGLE * FROM  Y1ig_invrefnum_dd WHERE  Docno = @billingdocument    INTO @DATA(irn_deatils)   .

*    Shipping Partner Details
    SELECT SINGLE * FROM  I_BillingDocumentPartner AS a  INNER JOIN i_customer as b ON   ( a~Customer = b~Customer  )
                         WHERE a~BillingDocument = @billingdocument AND a~PartnerFunction = 'WE' INTO @DATA(Shippingadd)   .
*    Dispatch Details
     SELECT SINGLE * FROM ZSD_PLANT_ADDRESS as a INNER JOIN I_BILLINGDOCUMENTITEMBASIC AS b ON ( a~Plant =  b~Plant )
                          WHERE BillingDocument =  @billingdocument into @data(plantaddress) .

*    ************Billing partener "BUYER'
     select single * from   I_BillingDocumentPartner as a  inner JOIN i_customer as
                 b on   ( a~Customer = b~Customer  ) where a~BillingDocument = @billingdocument
                  and a~PartnerFunction = 'RE' into  @DATA(buyeradd)   .

      GV3 = INVOICEDATE-BillingDocumentDate+0(4)  .
      GV2 = INVOICEDATE-BillingDocumentDate+4(2)  .
      GV1 = INVOICEDATE-BillingDocumentDate+6(2)  .
     CONCATENATE GV1 '/' GV2 '/' GV3 INTO GV .

     DATA  trsprt type Ytransport_det   .
     DATA : shipdtls TYPE Yei_shipdtls_tt.
     DATA : dispdtls TYPE Yei_dispdtls_t1.
     DATA : it_data2 TYPE STANDARD TABLE OF Yinv_stu,
               wa_data2 TYPE  Yinv_stu.
     DATA     irn   type string .
     DATA     ejson type string .

     DATA :   line1 type string value '[{"Irn":" ' ,                      "IRN..............
              line2 type string value  '","Distance":' ,
              line3 type string value  ',"TransMode":"' ,
              line4 type string value  '","TransId":"' ,
              line5 type string value  '","TransName":"' ,
              line6 type string value  '","TransDocDt":"' ,
              line7 type string value  '","TransDocNo":"' ,
              line8 type string value  '","VehNo":"' ,
              line9 type string value  '","VehType":"' ,
              line10 type string value  '","ExpShipDtls":{"Addr1":"' ,     "ExpShipDtls......
              line11 type string value  '","Addr2":"',
              line12 type string value  '","Loc":"',
              line13 type string value  '","Pin":',
              line14 type string value  ',"Stcd":"',
              line15 type string value  '"},"DispDtls": {"Nm": "',         "DispDtls.........
              line16 type string value  '","Addr1":"' ,
              line17 type string value  '","Addr2":"' ,
              line18 type string value  '","Loc":"',
              line19 type string value  '","Pin":',
              line20 type string value  ',"Stcd":"',
              line21 type string value  '"}}]'.

              data  : vehitype type  string .
              data  : kjj type  string .



    """"""""""""""""""""""""""""""""""""""""""""""""""Postal & State Code for Port Of Loading""""""""""""""""""""""""""""""""""""""""
        SELECT SINGLE * FROM i_billingdocumentbasic WHERE billingdocument = @billingdocument INTO @DATA(portdata)  .
        SELECT SINGLE * FROM I_BillingDocumentItem WHERE billingdocument = @billingdocument INTO @DATA(portdata2)  .
            SELECT SINGLE * FROM I_DeliveryDocument WHERE DeliveryDocument = @portdata2-ReferenceSDDocument INTO @DATA(SHIP).

       trsprt-transdocno    =    transportdoc .
       trsprt-transid       =    transportid .
       trsprt-transname     =    transpoter_name .
       IF INVOICEDATE-YY1_VehicleNo_BDH IS NOT INITIAL .
       trsprt-transdocdt    =  GV .
       ENDIF.
       IF INVOICEDATE-YY1_VehicleNo_BDH IS INITIAL.
        trsprt-vehno       = vehiclenumber.
        ELSE.
        trsprt-vehno          =    INVOICEDATE-YY1_VehicleNo_BDH. "vehiclenumber  .
        ENDIF.
      if SHIP-ShippingType <> ''.
      SHIFT  SHIP-ShippingType LEFT DELETING LEADING '0' .
      else.
      SHIP-ShippingType = '1'.
      ENDIF.
       trsprt-transmode     =     SHIP-ShippingType . " INVOICEDATE-YY1_TransportMode_BDH . "inv'1'   .
"       trsprt-transmode     =      |{ INVOICEDATE-YY1_TransportMode_BDH ALPHA = out }| . " INVOICEDATE-YY1_TransportMode_BDH . "inv'1'   .
       trsprt-vehtype       =    'R'  .
       irn                  =    irn_deatils-Irn .

    SELECT SINGLE * FROM zpregen_exi WHERE docno = @billingdocument AND Doctype = 'PO'  INTO @DATA(portdata1) .

        DATA: Postal    TYPE STRING,
              LOC       TYPE STRING,
              StateCode TYPE STRING.

      Postal    =   portdata1-POLPinCode .
      StateCode =   portdata1-POLStCode.
      LOC       =   portdata1-Portofloading.


""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
      IF ( INVOICEDATE-distributionchannel = '17' ) AND buyeradd-b-country NE 'IN' .

         IF buyeradd-b-country NE 'IN' AND buyeradd-b-taxnumber3 IS   INITIAL.
           wa_data2-taxsch     =  'GST'.
           wa_data2-version    =  '1.1'.
           wa_data2-irn        =  'GST'.
            IF INVOICEDATE-CustomerGroup = '01'.
            wa_data2-suptyp     = 'EXPWP'.
            ELSEIF INVOICEDATE-CustomerGroup = '02'.
            wa_data2-suptyp     = 'EXPWOP'.
            ENDIF.
            wa_data2-regrev     =  'N'.
            wa_data2-ecmgstin   =  ' '.
            "BUYER DETAILS
            shipdtls-Addr1 = Shippingadd-b-CustomerName .
            shipdtls-Addr2 = Shippingadd-b-StreetName .
            shipdtls-Loc   = LOC .
            shipdtls-Pin1   = Postal .
            shipdtls-Stcd  = StateCode .
          ENDIF .

         ELSE.
        wa_data2-taxsch     =  'GST'.
        wa_data2-version    =  '1.1'.
        wa_data2-irn        =  'GST'.
        wa_data2-suptyp     = 'B2B'.
        wa_data2-suptyp     = 'B2B'.
        wa_data2-regrev     =  'N'.
        wa_data2-ecmgstin   =  ' '.
        "BUYER DETAILS
        shipdtls-Addr1 = Shippingadd-b-CustomerName .
        shipdtls-Addr2 = Shippingadd-b-StreetName .
        shipdtls-Loc   = Shippingadd-b-CityName .
        shipdtls-Pin1   = Shippingadd-b-PostalCode .
        shipdtls-Stcd  = Shippingadd-b-Region .
        ENDIF.

        dispdtls-Nm    = plantaddress-a-PlantName .
        dispdtls-Addr1 = plantaddress-a-AddresseeFullName .
        dispdtls-Addr2 = plantaddress-a-streete.
        dispdtls-Loc   = plantaddress-a-city .
        dispdtls-Pin1   = plantaddress-a-post .
        if plantaddress-a-post  = 'GJ'.
        dispdtls-Stcd  = '24' .
        else.
        dispdtls-Stcd  = '08' .
        ENDIF.


      IF distance IS INITIAL.
        SELECT SINGLE * FROM  Y1ig_invrefnum WHERE docno = @billingdocument  INTO @DATA(DIST)   .

      if   trsprt-transid  is initial .

     CONCATENATE line1 irn line2 dist-distance line3 trsprt-transmode line4 'null' line5 trsprt-transname
     line6 trsprt-transdocdt line7 trsprt-transdocno line8 trsprt-vehno line9 trsprt-vehtype line10 shipdtls-Addr1
     line11 shipdtls-Addr2 line12 shipdtls-Loc line13 shipdtls-Pin1 line14 shipdtls-Stcd line15 dispdtls-Nm
     line16 dispdtls-Addr1 line17 dispdtls-Addr2 line18 dispdtls-Loc line19 dispdtls-Pin1 line20 dispdtls-Stcd
     line21  into ejson .
        REPLACE ALL OCCURRENCES OF '"null"' IN ejson WITH 'null'.
     CONDENSE EJSON.

      ELSE .

     CONCATENATE line1 irn line2 dist-distance line3 trsprt-transmode line4 trsprt-transid  line5 trsprt-transname
     line6 trsprt-transdocdt line7 trsprt-transdocno line8 trsprt-vehno line9 trsprt-vehtype line10 shipdtls-Addr1
     line11 shipdtls-Addr2 line12 shipdtls-Loc line13 shipdtls-Pin1 line14 shipdtls-Stcd line15 dispdtls-Nm
     line16 dispdtls-Addr1 line17 dispdtls-Addr2 line18 dispdtls-Loc line19 dispdtls-Pin1 line20 dispdtls-Stcd
     line21  into ejson .
     CONDENSE EJSON.

      ENDIF .

    ELSE.

      if   trsprt-transid  is initial .

     CONCATENATE line1 irn line2 distance line3 trsprt-transmode line4 'null' line5 trsprt-transname
     line6 trsprt-transdocdt line7 trsprt-transdocno line8 trsprt-vehno line9 trsprt-vehtype line10 shipdtls-Addr1
     line11 shipdtls-Addr2 line12 shipdtls-Loc line13 shipdtls-Pin1 line14 shipdtls-Stcd line15 dispdtls-Nm
     line16 dispdtls-Addr1 line17 dispdtls-Addr2 line18 dispdtls-Loc line19 dispdtls-Pin1 line20 dispdtls-Stcd
     line21  into ejson .
        REPLACE ALL OCCURRENCES OF '"null"' IN ejson WITH 'null'.
     CONDENSE EJSON.

      else .

     CONCATENATE line1 irn line2 distance line3 trsprt-transmode line4 trsprt-transid  line5 trsprt-transname
     line6 trsprt-transdocdt line7 trsprt-transdocno line8 trsprt-vehno line9 trsprt-vehtype line10 shipdtls-Addr1
     line11 shipdtls-Addr2 line12 shipdtls-Loc line13 shipdtls-Pin1 line14 shipdtls-Stcd line15 dispdtls-Nm
     line16 dispdtls-Addr1 line17 dispdtls-Addr2 line18 dispdtls-Loc line19 dispdtls-Pin1 line20 dispdtls-Stcd
     line21  into ejson .
     CONDENSE EJSON.

      ENDIF .

    ENDIF.

    REPLACE ALL OCCURRENCES OF '"Distance":,' IN ejson WITH '"Distance": "0",'.

 DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = billingdocument companycode = companycode lv_string = ejson
                                                                               eway_generate = eway_generate  )  .
*******************************************************************************************************************
  REPLACE ALL OCCURRENCES OF '[{"TransId":"' IN result9  WITH '{"TransId":"'.

 FIELD-SYMBOLS:
           <dataX>              TYPE any,
           <dataY>              TYPE any,
           <dataZ>              TYPE any,
           <data2>              TYPE any,
           <data3>              TYPE any ,
           <field>              TYPE any ,
           <field_AckDt>        TYPE any,
           <field_AckNo>        TYPE any ,
           <field_Irn>          TYPE any ,
           <field_EwbNo>        TYPE any ,
           <field_EwbDt>        TYPE any ,
           <field_status>       TYPE any ,
           <field_Success>      TYPE any ,
           <field_Dis>          TYPE any ,
           <ERROR_MESSAGE>      TYPE any ,
         <field_EwbValidTill>   TYPE any .

     DATA  ACKDT   TYPE STRING .
     DATA  ACKNO   TYPE STRING .
     DATA  EWBNO   TYPE STRING .
     DATA  EWBDT   TYPE STRING .
     DATA  STATUS2 TYPE STRING .
     DATA  SUCCESS TYPE STRING .
     DATA  KM TYPE STRING .
     DATA  DIS TYPE STRING .
     DATA  EWBVALIDTILL TYPE STRING .
     DATA  ERROR_MESSAGE TYPE STRING .
     DATA: lr_str TYPE REF TO data.

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""for distance
TYPES: BEGIN OF ty_info,
         infcd TYPE string,
         desc  TYPE string,
       END OF ty_info.

TYPES: tt_info TYPE STANDARD TABLE OF ty_info WITH EMPTY KEY.

TYPES: BEGIN OF ty_govt_response,
         info          TYPE tt_info,
       END OF ty_govt_response.

TYPES: BEGIN OF ty_payload,
         govt_response TYPE ty_govt_response,
       END OF ty_payload.
DATA(ls_payload) = VALUE ty_payload( ).
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

        DATA respo         TYPE  Yewayrescoll .
        DATA EWAYBILLDATA  TYPE  yj1ig_ewaybill .

     DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).

       IF lr_d1 IS BOUND.
       ASSIGN lr_d1->* TO <dataX>.
       ASSIGN COMPONENT `ERROR_MESSAGE` OF STRUCTURE <dataX> TO <ERROR_MESSAGE> .
       if sy-subrc = 0 .
       ASSIGN <ERROR_MESSAGE>->* TO <ERROR_MESSAGE>  .
       ERROR_MESSAGE = <ERROR_MESSAGE> .
       ENDIF.
     IF ERROR_MESSAGE IS INITIAL .
       ASSIGN COMPONENT `GOVT_RESPONSE` OF STRUCTURE <dataX> TO <field> .
       IF sy-subrc <> 0.
              LOOP AT <datax> ASSIGNING FIELD-SYMBOL(<fs>).
                ASSIGN <fs>->* TO FIELD-SYMBOL(<fs1>) .
                ASSIGN COMPONENT `GOVT_RESPONSE` OF STRUCTURE <fs1> TO <field>    .
                EXIT.
              ENDLOOP.
      ENDIF.

        if sy-subrc = 0 .
        ASSIGN <field>->* TO <data2>  .
        if sy-subrc = 0 .

        ASSIGN COMPONENT `Irn` OF STRUCTURE <data2> TO <field_Irn>    .
         if sy-subrc = 0 .
        ASSIGN <field_Irn>->* TO <field_Irn> .
        IRN = <field_Irn> .
        ENDIF.
        ASSIGN COMPONENT `EwbNo` OF STRUCTURE <data2>  TO   <field_EwbNo>     .
        if sy-subrc = 0 .
        ASSIGN  <field_EwbNo>->* TO  <field_EwbNo>  .
        EWBNO  = <field_EwbNo> .
       ENDIF.
        ASSIGN COMPONENT `EwbDt` OF STRUCTURE <data2>  TO   <field_EwbDt>     .
         if sy-subrc = 0 .
        ASSIGN  <field_EwbDt>->* TO  <field_EwbDt>  .
        EWBDT  = <field_EwbDt> .
        ENDIF.
        ASSIGN COMPONENT `status` OF STRUCTURE <data2>  TO   <field_status>     .
         if sy-subrc = 0 .
        ASSIGN  <field_status>->* TO  <field_status>  .
        STATUS2  = <field_status> .
       ENDIF.
        ASSIGN COMPONENT `Success` OF STRUCTURE <data2>  TO   <field_Success>     .
         if sy-subrc = 0 .
        ASSIGN  <field_Success>->* TO  <field_Success>  .
        SUCCESS  = <field_Success> .
       ENDIF.
        ASSIGN COMPONENT `EWBVALIDTILL` OF STRUCTURE <data2>  TO   <field_EwbValidTill>     .
       if sy-subrc = 0 .
        ASSIGN  <field_EwbValidTill>->* TO  <field_EwbValidTill>  .
        EWBVALIDTILL  = <field_EwbValidTill> .
       ENDIF.

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""""""""""""""""for distance""""""""""""""""""""
        /ui2/cl_json=>deserialize(
         EXPORTING
           json = result9
         CHANGING
           data = ls_payload
       ).
       DATA lv_distance TYPE N LENGTH 10 .
        LOOP AT ls_payload-govt_response-info  INTO DATA(ls_value).

         FIND REGEX '(\d+)' IN ls_value-desc SUBMATCHES lv_distance.

        EWAYBILLDATA-distance = lv_distance.
        MODIFY yj1ig_ewaybill FROM @EWAYBILLDATA  .
        ENDLOOP.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
       ASSIGN COMPONENT `alert` OF STRUCTURE <data2>  TO  FIELD-SYMBOL(<km>)     .
            IF sy-subrc = 0.
              IF <km> IS BOUND.
                km = reverse( <km>->* ).
                SPLIT km+1(*) AT '' INTO km DATA(other).
                km = km+1(*).
                km = reverse( km ).
              ENDIF.
            ENDIF.

     ENDIF.
     ENDIF.
     ENDIF.

    """""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""

     IF ewbno IS NOT INITIAL.
       SELECT SINGLE SDDocumentCategory, companycode FROM I_BillingDocumentBasic
             WHERE BillingDocument = @billingdocument  INTO (@DATA(bill_typ),@DATA(comp) )   .

      IF bill_typ EQ 'M' .
            wa_data2-i_typ = 'INV'.
           wa_data2-suptyp     = 'B2B'.
      ELSEIF bill_typ = 'O'.
            wa_data2-i_typ = 'CRN'.
        wa_data2-suptyp     = 'B2B'.
      ELSEIF bill_typ = 'P'.
            wa_data2-i_typ = 'DBN'.
         wa_data2-suptyp     = 'B2B'.
      ELSEIF bill_typ = 'U'.
            wa_data2-i_typ = 'PINV'.
      ENDIF.

      EWAYBILLDATA-bukrs       =  comp.
      EWAYBILLDATA-doctyp      =  wa_data2-i_typ.
      EWAYBILLDATA-docno       =  Billingdocument  .
      EWAYBILLDATA-gjahr       =  SY-datum+0(4)  .
      EWAYBILLDATA-ebillno     =  ewbno .
      EWAYBILLDATA-egen_dat    =  SY-DATUM .
      EWAYBILLDATA-status      =  'ACT'   .
      EWAYBILLDATA-vehiclenum  =  vehiclenumber .

       DATA distance1 TYPE STRING.
      IF distance IS INITIAL .
      distance1 = KM.
      ELSE.
      distance1 = distance.
      ENDIF.

      EWAYBILLDATA-distance    =  distance1 .
      EWAYBILLDATA-transporter = transpoter_name .
      EWAYBILLDATA-transportid = transportid .

      DATA(USERID) = cl_abap_context_info=>get_user_technical_name( ) .
      EWAYBILLDATA-createdby = USERID.

      IF EWBVALIDTILL IS NOT INITIAL .
      ewaybilldata-vdtodate    =  EWBVALIDTILL+0(4) && EWBVALIDTILL+5(2) && EWBVALIDTILL+8(2).
      ENDIF.

     MODIFY yj1ig_ewaybill FROM @EWAYBILLDATA  .
          COMMIT WORK AND WAIT.

     status = | Eway Bill { STATUS2 } Suscessfully , Eway Bill No. { EWBNO } ' ' Eway Bill Date { EWBDT }  | .

      ELSE .

       final_message_ewaybill = result9 .
      status = final_message_ewaybill .
      ENDIF.

      ENDIF.

  ENDMETHOD.
ENDCLASS.
