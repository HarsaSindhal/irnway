CLASS zsd_irn_cancel DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .
  PUBLIC SECTION.
     "Class for Cancel Irn AND EWAY-BILL_CANCEL
      CLASS-METHODS :
      cancel_irn
        IMPORTING invvoice          TYPE char10 OPTIONAL
                  companycode       TYPE char4 OPTIONAL
                  Year              TYPE string OPTIONAL
                  invtype           TYPE STRING OPTIONAL
                  VALUE(Cancelirn)  TYPE string

        RETURNING VALUE(status) TYPE string ,
       EWAY_BILL_CANCEL
        IMPORTING invvoice          TYPE char10 OPTIONAL
                  companycode       TYPE char4 OPTIONAL
                  VALUE(CancelEway_Bill)  TYPE string

        RETURNING VALUE(status) TYPE string .
         CLASS-DATA : final_Message_Irn TYPE string .
    CLASS-DATA : final_Message_ewaybill TYPE string .
    CLASS-DATA : final_Message_ewaybill2 TYPE string .
  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.



CLASS ZSD_IRN_CANCEL IMPLEMENTATION.


METHOD cancel_irn.
 TYPES: BEGIN OF ty_result,
           str TYPE c LENGTH 5000,
         END   OF ty_result.

    TYPES: BEGIN OF ty_scope,
           scope TYPE string,
         END OF ty_scope.
  TYPES: BEGIN OF ty_response_auth,
           access_token(60).
           INCLUDE TYPE ty_scope.
     TYPES: resourceownerid(60),
           token_type(40),
           orgid(60),
         END OF ty_response_auth.
   DATA: lt_itab_result TYPE TABLE OF ty_result,
        gs_resp_auth   TYPE ty_response_auth.

   data : eway type string .
   data : json1 type string .
   data : json type string .


   TYPES: BEGIN OF ty_auth,
             access_token TYPE string,
             token_type   TYPE string,
             expires_in   TYPE string,
             scope        TYPE string,
             jti          TYPE string,
           END OF ty_auth.


   DATA: lv_service_url     TYPE string,
          lv_http_status     TYPE i,
          lv_token           TYPE string,
          ls_json            TYPE string,
          ls_json_temp       TYPE string .

    DATA reqid TYPE string.
    DATA : ls_service_url TYPE string.
    DATA lv_response TYPE string.
    DATA ls_test_content TYPE string.
    DATA: auth_json TYPE ty_auth.

 IF invvoice IS NOT INITIAL.

    data(bracket1_o) = '[' .
    data(bracket2_o) = '{' .
    data(bracket1_c) = ']' .
    data(bracket2_c) = '}' .
    data(cm)     = '"' .
    data veria type string .
    data : inv_voice  type c LENGTH 10.
           inv_voice = invvoice.
    inv_voice = |{ inv_voice alpha = in }|.

IF  invtype = 'Finance Invoice' .
 SELECT SINGLE * FROM  y1ig_invrefnum where docno = @inv_voice AND bukrs = @companycode AND doc_year = @year INTO @DATA(irn_BILLDATA)  .
ELSE.
 SELECT SINGLE * FROM  y1ig_invrefnum where docno = @inv_voice AND bukrs = @companycode INTO @irn_BILLDATA  .
ENDIF.

  json1 = |{ bracket1_o }{ bracket2_o }{ cm }irn{ cm }:{ cm }{ irn_BILLDATA-irn }{ cm },| && |"CnlRsn": "2","CnlRem": "Data entry mistake" { bracket2_c }{ bracket1_c }|         .

 DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invvoice companycode = CompanyCode
                                                                            lv_string = json1 irncancel = cancelirn year = year )  .

 FIELD-SYMBOLS:
    <data>                TYPE data,
    <data1>                TYPE data,
    <data2>                TYPE data,
    <field>               TYPE any .

FIELD-SYMBOLS:
       <datacan>               TYPE any,
       <datacan2>              TYPE data,
       <datacan3>              TYPE data,
       <datacan4>              TYPE any ,
       <fieldcan>              TYPE any ,
       <field_ackdtcan>        TYPE any,
       <field_acknocan>        TYPE any ,
       <field_irncan>          TYPE any ,
       <field_status>       TYPE any ,
       <CANCELDATE>       TYPE any ,
       <field_success>      TYPE any .

    DATA IRNCAN TYPE STRING .
    DATA ACKDTCAN TYPE STRING .
    DATA ACKNOCAN TYPE STRING .
    DATA SUCCESSCAN TYPE STRING .
    DATA CANCELDATE TYPE STRING .
DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).

   IF lr_d1 IS BOUND.

    ASSIGN lr_d1->* TO <datacan>.

  LOOP AT <datacan> ASSIGNING FIELD-SYMBOL(<FS111>)  .

    ASSIGN <FS111>->* TO <datacan3>.

    ASSIGN COMPONENT `DOCUMENT_STATUS` OF STRUCTURE  <datacan3>  TO   <field_status>     .
    if sy-subrc = 0 .
    ASSIGN <field_status>->* TO <field_status>   .
    if sy-subrc = 0 .
    SUCCESSCAN = <field_status> .
    ENDIF.
    ENDIF.

    ASSIGN COMPONENT `GOVT_RESPONSE` OF STRUCTURE <datacan3>  TO  <datacan4>    .
    if sy-subrc = 0 .
    ASSIGN <datacan4>->* TO <datacan2> .
    if sy-subrc = 0 .

    ASSIGN COMPONENT `IRN` OF STRUCTURE <datacan2>  TO   <field_irncan>    .
    if sy-subrc = 0 .
    ASSIGN  <field_irncan>->* TO  <field_irncan> .
    if sy-subrc = 0 .
    IRNCAN =  <field_irncan> .
    ENDIF.
    ENDIF.

    ASSIGN COMPONENT `ACKNO` OF STRUCTURE <datacan2>  TO   <field_acknocan>     .
    if sy-subrc = 0 .
    ASSIGN  <field_acknocan>->* TO  <field_acknocan>  .
    if sy-subrc = 0 .
    ACKNOCAN =  <field_acknocan>  .
    ENDIF.
    ENDIF.

    ASSIGN COMPONENT `ACKDT` OF STRUCTURE <datacan2>  TO   <field_ackdtcan>    .
    if sy-subrc = 0 .
    ASSIGN  <field_ackdtcan>->* TO  <field_ackdtcan> .
    if sy-subrc = 0 .
    ACKDTCAN =  <field_ackdtcan> .
    ENDIF.
    ENDIF.

    ASSIGN COMPONENT `CANCELDATE` OF STRUCTURE <datacan2>  TO   <CANCELDATE>    .
    if sy-subrc = 0 .
    ASSIGN  <CANCELDATE>->* TO  <CANCELDATE> .
    if sy-subrc = 0 .
    CANCELDATE =  <CANCELDATE> .
    ENDIF.
    ENDIF.

   ENDIF.
   ENDIF.

   ENDLOOP .
   ENDIF.

      IF IRNCAN IS NOT INITIAL.

      irn_BILLDATA-irn            = IRNCAN .
      irn_BILLDATA-cancel_date    = sy-datum .
      irn_BILLDATA-irn_status     = 'CANCEL'   .

      MODIFY y1ig_invrefnum FROM  @irn_BILLDATA   .
      COMMIT WORK AND WAIT.

      final_message_irn = SUCCESSCAN .
      status = final_message_irn .

    ELSE .
      final_message_ewaybill = result9 .
      status = final_message_ewaybill .

    ENDIF  .
  ENDIF.
  ENDMETHOD.


  METHOD EWAY_BILL_CANCEL.


   TYPES: BEGIN OF ty_result,
           str TYPE c LENGTH 5000,
         END   OF ty_result.

    TYPES: BEGIN OF ty_scope,
           scope TYPE string,
         END OF ty_scope.
  TYPES: BEGIN OF ty_response_auth,
           access_token(60).
           INCLUDE TYPE ty_scope.
     TYPES: resourceownerid(60),
           token_type(40),
*           expires_in          TYPE p DECIMALS 0,
           orgid(60),
         END OF ty_response_auth.
   DATA: lt_itab_result TYPE TABLE OF ty_result,
        gs_resp_auth   TYPE ty_response_auth.

   data : eway type string .
   data : json1 type string .
   data : json type string .

eway = invvoice .



   DATA: lv_service_url     TYPE string,
          lv_http_status     TYPE i,
          lv_token           TYPE string,
          ls_json            TYPE string,
          ls_json_temp       TYPE string .

data: x1 type string value '{'  .
data: x2 type string value '}'  .


 SELECT SINGLE * FROM  yj1ig_ewaybill where docno = @invvoice  INTO @DATA(Eway_BILLDATA)  .

    IF eway IS NOT INITIAL.

   json1 =   | "ewbNo" : | && Eway_BILLDATA-ebillno && | ,"cancelRsnCode": "DATA_ENTRY_MISTAKE","cancelRmrk": "DATA_ENTRY_MISTAKE" |         .

 CONCATENATE x1  json1  x2 into json1 .

   else .

data: v1 type string value   '{"irn": "' ,
      v2 type string value '","cnlrsn": "1","cnlrem": "wrong entry" }'  .

  CONCATENATE v1 eway v2 into json1 .


   ENDIF.

    DATA EWAYBILLDATA TYPE yj1ig_ewaybill .
  DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invvoice companycode = CompanyCode
                                                                            lv_string = json1 eway_cancel = CancelEway_Bill )  .

 FIELD-SYMBOLS:

       <datacan2>              TYPE data,
       <errorDetails1>     TYPE any ,
       <field_EWAYBILLcan>     TYPE any ,
       <errorDetails>     TYPE any ,
       <errorDemassage>     TYPE any ,
       <field_status>          TYPE any ,
       <CANCELDATE>            TYPE any ,
       <field_success>         TYPE any .

    DATA EWAYBILLCAN     TYPE STRING .
    DATA ACKDTCAN   TYPE STRING .
    DATA ACKNOCAN   TYPE STRING .
    DATA SUCCESSCAN TYPE STRING .
    DATA errormassage   TYPE STRING .

    DATA(lr_d1) = /ui2/cl_json=>generate( json = result9 ).

    IF lr_d1 IS BOUND.
    ASSIGN lr_d1->* TO <datacan2>.

    ASSIGN COMPONENT `ewbNumber` OF STRUCTURE <datacan2>  TO   <field_EWAYBILLcan>    .
    if sy-subrc = 0 .
    ASSIGN  <field_EWAYBILLcan>->* TO  <field_EWAYBILLcan> .
    EWAYBILLCAN =  <field_EWAYBILLcan> .
    ENDIF.
    ASSIGN COMPONENT `ewbStatus` OF STRUCTURE <datacan2>  TO   <field_success>     .
    if sy-subrc = 0 .
    ASSIGN  <field_success>->* TO  <field_success>  .
    if  sy-subrc = 0 .
    SUCCESSCAN =  <field_success>  .
    else.
    ASSIGN COMPONENT `errorDetails` OF STRUCTURE <datacan2>  TO  <errorDetails>    .
    if sy-subrc = 0 .
    ASSIGN  <errorDetails>->* TO  <errorDetails1>  .
    if  sy-subrc = 0 .
    ASSIGN COMPONENT `error_message` OF STRUCTURE <errorDetails1>  TO  <errorDemassage>    .
    if  sy-subrc = 0 .
    ASSIGN  <errorDemassage>->* TO  <errorDemassage>  .
    if  sy-subrc = 0 .
    errormassage = <errorDemassage> .
    ENDIF.
    ENDIF.
    ENDIF.
    ENDIF.
    ENDIF.
    ENDIF.
    ENDIF.



     IF EWAYBILLCAN IS NOT INITIAL AND errormassage  is INITIAL.
       SELECT SINGLE SDDocumentCategory, companycode FROM I_BillingDocumentBasic
             WHERE BillingDocument = @invvoice  INTO (@DATA(bill_typ),@DATA(comp) )   .

     SELECT SINGLE * FROM yj1ig_ewaybill WHERE  docno = @invvoice INTO @DATA(BILLDSTA).
      EWAYBILLDATA-bukrs       =  comp.
      EWAYBILLDATA-doctyp      =  BILLDSTA-doctyp.
      EWAYBILLDATA-docno       =  invvoice  .
      EWAYBILLDATA-gjahr       =  BILLDSTA-gjahr  .
      EWAYBILLDATA-ebillno     =  EWAYBILLCAN .
      EWAYBILLDATA-status      =  'CAN' .

     MODIFY yj1ig_ewaybill FROM @EWAYBILLDATA  .
          COMMIT WORK AND WAIT.

     status = SUCCESSCAN .

       ELSE .

       final_message_ewaybill = errormassage .
      status = final_message_ewaybill .
      ENDIF.
  ENDMETHOD.
ENDCLASS.
