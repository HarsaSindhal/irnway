CLASS zsd_authentication_token_irn DEFINITION
  PUBLIC  "    "
  FINAL  "    "
  CREATE PUBLIC .
  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun . "    "
     CLASS-DATA : access_token TYPE string .
     CLASS-METHODS :
     generate_authentication_token
        IMPORTING billingdocument TYPE CHAR10  OPTIONAL
                  companycode     TYPE CHAR4  OPTIONAL
                  lv_string       TYPE STRING  OPTIONAL
                  irngenrate      TYPE STRING  OPTIONAL
                  eway_generate   TYPE STRING  OPTIONAL
                  irnCancel       TYPE STRING  OPTIONAL
                  eway_Cancel     TYPE STRING  OPTIONAL
                  year            TYPE STRING  OPTIONAL
                  INTYPE          TYPE STRING  OPTIONAL
                  lv_all_a0       type  abap_bool OPTIONAL
        RETURNING VALUE(auth_token) TYPE string ,
        "    "
      create_client
        IMPORTING url           TYPE string
        RETURNING VALUE(result) TYPE REF TO if_web_http_client
        RAISING   cx_static_check .
  PROTECTED SECTION.
  "    "
  PRIVATE SECTION.
  "    "
ENDCLASS.



CLASS ZSD_AUTHENTICATION_TOKEN_IRN IMPLEMENTATION.


  METHOD create_client.       "    "
    DATA(dest) = cl_http_destination_provider=>create_by_url( url ).
    result = cl_web_http_client_manager=>create_by_http_destination( dest ).
  ENDMETHOD.


 METHOD generate_authentication_token.
   "*******************************************************************************Generate_irn........
      SELECT SINGLE PLANT , Billingdocumenttype,DistributionChannel,SalesOrganization FROM I_BillingDocumentItem   WHERE
     BillingDocument  =  @billingdocument  INTO  @DATA(plant)  .
   seleCT sinGLE * from Y1ig_invrefnum WITH PRIVILEGED ACCESS whERE docno = @billingdocument into @data(irncheck).
if plant is INITIAL .
 select * from i_operationalacctgdocitem
   where accountingdocument = @billingdocument AND TaxItemAcctgDocItemRef is not INITIAL
    and AccountingDocumentItemType <> 'T' AND CompanyCode = @companycode AND FiscalYear = @year
    AND ProfitCenter <> '' into table @data(i_billingpart2).

  loop at i_billingpart2 into data(wa_bill).
   data(wabill1) = wa_bill.
  endloop.

data(hsn_sac) = wa_bill-IN_HSNOrSACCode.
"""""""""""""""""""""""""""""""""""""""""""FETCHING PLANT FROM PROFIT CENTER"""""""
data plant1 type c LENGTH 4.
data var2   type c LENGTH 6.
 data(g_l2) = strlen( WABILL1-ProfitCenter ).
 var2       = WABILL1-ProfitCenter+6(4).
 plant-Plant     = var2.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
ENDIF.

     DATA lv_service_url TYPE string.
         DATA:uuid TYPE string.
    data:user_name TYPE string,
         password  TYPE string,
         gstin_d   type string,
         token     TYPE string.

    if sy-sysiD = 'ALU'  OR SY-SYSID = 'AWL'.
    IF irngenrate = 'X' .
    lv_service_url = 'https://api-sandbox.clear.in/einv/v2/eInvoice/generate'.

    ELSEIF eway_generate = 'X'.

      if plant-BillingDocumentType = 'JSN' OR plant-BillingDocumentType = 'F8' OR
     ( ( plant-BillingDocumentType = 'F2' OR plant-BillingDocumentType = 'JSTO' ) and plant-SalesOrganization = '3000' )  OR
       ( plant-BillingDocumentType = 'F2' and plant-SalesOrganization = '1000' and irncheck-irn is iniTIAL ) or
       ( plant-BillingDocumentType =  'JSTO' and lv_all_a0 = 'X' )
         OR intype = 'Purchase Return'.

      lv_service_url       =  'https://api-sandbox.clear.in/einv/v3/ewaybill/generate' .
      ELSE.
      lv_service_url       =  'https://api-sandbox.clear.in/einv/v2/eInvoice/ewaybill' .
      ENDIF.

    ELSEIF eway_cancel   = 'X'.
      lv_service_url       =  'https://api-sandbox.clear.in/einv/v2/eInvoice/ewaybill/cancel' .

    ELSEIF irncancel     = 'X'.
      lv_service_url       =  'https://api-sandbox.clear.in/einv/v2/eInvoice/cancel' .

    ENDIF.
    CONDENSE: lv_service_url.

        USER_NAME = 'cleartax08'.
        Password  = 'cleartaxtest@08'.
        GSTIN_D   = '08AAFCD5862R018'.
        token     = 'sandbox.3.1a4cace2-95aa-404a-8cba-ccdffb3cab60_64d9d8792317a543e97606345ca64f8e951e60e7042d547e5886267a98ed13bc'.
    uuid = cl_system_uuid=>create_uuid_x16_static(  ).

    DATA(url) = |{ lv_service_url }|.
    DATA(client) = create_client( url ).
    DATA(req) = client->get_http_request(  ).

    req->set_header_field( i_name = 'user_id' i_value =  USER_NAME  ).
    req->set_header_field( i_name = 'password' i_value = Password ).
    req->set_header_field( i_name = 'gstin'    i_value = GSTIN_D ).
    req->set_header_field( i_name = 'requestid' i_value = uuid ).
    req->set_header_field( i_name = 'X-Cleartax-Auth-Token'    i_value = token ).
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""



 ELSEIF  sy-sysiD = 'D9S' .

  IF irngenrate = 'X' .
    lv_service_url = 'https://api.clear.in/einv/v2/eInvoice/generate'.

  ELSEIF eway_generate = 'X' .

     if plant-BillingDocumentType = 'JSN' OR plant-BillingDocumentType = 'F8'   OR
    ( ( plant-BillingDocumentType = 'F2' OR plant-BillingDocumentType = 'JSTO' )  and plant-SalesOrganization = '3000' ) OR
      ( plant-BillingDocumentType = 'F2' and plant-SalesOrganization = '1000' and irncheck-irn is iniTIAL )   or
      ( plant-BillingDocumentType =  'JSTO' and lv_all_a0 = 'X' )
        OR intype = 'Purchase Return'.

      lv_service_url       =  'https://api.clear.in/einv/v3/ewaybill/generate' .
      ELSE.
      lv_service_url       =  'https://api.clear.in/einv/v2/eInvoice/ewaybill' .
      ENDIF.

  ELSEIF eway_cancel   = 'X'.
      lv_service_url       =  'https://api.clear.in/einv/v2/eInvoice/ewaybill/cancel' .

  ELSEIF irncancel     = 'X'.
      lv_service_url       =  'https://api.clear.in/einv/v2/eInvoice/cancel' .

  ENDIF.
   CONDENSE: lv_service_url.
"""""""""""""""""""""""""""PLANT LOGIC""""""""""""""""""""""""""""""""""""""""""""""""""""

SELECT SINGLE * FROM zsales_tmg WHERE plant = @PLANT-Plant INTO @data(irngst).


        USER_NAME = irngst-id.
        Password  = irngst-password.
        GSTIN_D   = irngst-gstno.
        token     = '3.aa0ee9a8-d5ad-4e0c-a7b2-e721f50eab8e_7967787bfb61cc0c0c8670668322ad9bad7b875dbd0ce5717e0cb911389c89ed'.

   uuid = cl_system_uuid=>create_uuid_x16_static(  ).
    url = |{ lv_service_url }|.
    client = create_client( url ).
    req = client->get_http_request(  ).
    req->set_header_field( i_name = 'user_id' i_value =  USER_NAME  ).
    req->set_header_field( i_name = 'password' i_value = Password ).
    req->set_header_field( i_name = 'gstin'    i_value = GSTIN_D ).
    req->set_header_field( i_name = 'X-Cleartax-Auth-Token'    i_value = token ).
    req->set_header_field( i_name = 'requestid' i_value = uuid ).
    CONCATENATE 'X-Cleartax-Auth-Token'  access_token INTO access_token SEPARATED BY space.

 ENDIF.

    req->set_header_field( i_name = 'Authorization' i_value = access_token ).

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
    req->set_content_type( 'application/json' ).
    req->set_text( lv_string ) .
    DATA: result9 TYPE string.
    if  eway_generate = 'X' OR eway_cancel   = 'X'.

     if eway_cancel <> 'X' AND ( plant-BillingDocumentType = 'JSN' OR plant-BillingDocumentType = 'F8' OR
    ( ( plant-BillingDocumentType = 'F2' OR plant-BillingDocumentType = 'JSTO' ) and plant-SalesOrganization = '3000' ) OR
      ( plant-BillingDocumentType = 'F2' and plant-SalesOrganization = '1000' and irncheck-irn is iniTIAL )  or
      ( plant-BillingDocumentType =  'JSTO' and lv_all_a0 = 'X' )
       OR intype = 'Purchase Return' ).

      result9 = client->execute( if_web_http_client=>put )->get_text( ).
      ELSE.
      result9 = client->execute( if_web_http_client=>Post )->get_text( ).
      ENDIF.

    else.
    result9 = client->execute( if_web_http_client=>put )->get_text( ).
    ENDIF.
    client->close(  ) .
    auth_token = result9 .
   "    "
  ENDMETHOD.
ENDCLASS.
