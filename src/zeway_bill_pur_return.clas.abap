CLASS zeway_bill_pur_return DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .

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


    CLASS-METHODS :
      get_table_fields
        IMPORTING VALUE(invoice)     TYPE CHAR10
                  VALUE(companycode) TYPE CHAR4
                  VALUE(year)        TYPE string
                  VALUE(Eway_generate)  TYPE string
                  VALUE(invoice1)     TYPE CHAR14
                  INTYPE    TYPE string OPTIONAL
                transpoter_name TYPE string OPTIONAL
                transportid     TYPE string OPTIONAL
                transportdoc    TYPE string OPTIONAL
                vehiclenumber   TYPE string OPTIONAL
                distance        TYPE string OPTIONAL
        RETURNING VALUE(result)     TYPE string.
  PROTECTED SECTION.

  PRIVATE SECTION.

ENDCLASS.



CLASS ZEWAY_BILL_PUR_RETURN IMPLEMENTATION.


  METHOD get_table_fields .
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


    DATA(lv_string)  =  zpur_return_invoice_irn_data=>generated_eway_bill( invoice = invoice year = year companycode = companycode invoice1 = invoice1
    transportdoc = transportdoc transportid = transportid transpoter_name = transpoter_name vehiclenumber = vehiclenumber distance = distance ) . .
    DATA(result9)  = zsd_authentication_token_irn=>generate_authentication_token( billingdocument = invoice companycode = CompanyCode lv_string = lv_string
                                                                             Eway_generate = Eway_generate intype = INTYPE  )  .

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
 UPDATE yj1ig_ewaybill SET vdtodate = '20250128'
                              WHERE ebillno = '781008911027'
                                AND docno   = '1700000044'.

        COMMIT WORK AND WAIT.
    IF w_ewaybill-ewbno IS NOT INITIAL.

select SINGLE * from i_operationalacctgdocitem
where LEFT( OriginalReferenceDocument,10 ) = @invoice1  AND FiscalYear = @year AND CompanyCode = @companycode INTO @DATA(documenttype) .

        ewaybilldata-bukrs       =  companycode.
        ewaybilldata-doctyp      =  documenttype-AccountingDocumentType.
        ewaybilldata-docno       =  invoice1  .
        ewaybilldata-gjahr       =  year  .
        ewaybilldata-ebillno     =  w_ewaybill-ewbno .
        ewaybilldata-egen_dat    =  sy-datum .
        ewaybilldata-status      =  'ACT'   .
        ewaybilldata-vehiclenum  =  vehiclenumber .

       dATA distance1 TYPE STRING.
      IF distance IS INITIAL .
      distance1 = w_ewaybill-km .
      ELSE.
      distance1 = distance.
      ENDIF.

      EWAYBILLDATA-distance    =  distance1 .
      EWAYBILLDATA-transporter = transpoter_name .
      EWAYBILLDATA-transportid = transportid .

      DATA(USERID) = cl_abap_context_info=>get_user_technical_name( ) .
      EWAYBILLDATA-createdby = USERID.

      IF i_eway-validupto IS NOT INITIAL .
      ewaybilldata-vdtodate    =  i_eway-validupto+0(4) && i_eway-validupto+5(2) && i_eway-validupto+8(2).
      ENDIF.

        MODIFY yj1ig_ewaybill FROM @ewaybilldata  .
        COMMIT WORK AND WAIT.


        DATA ewaypartb TYPE zeway_part_b .
        ewaypartb-docno       = invoice1 .
        ewaypartb-ewbnumber   = w_ewaybill-ewbno.
        ewaypartb-updateddate = i_eway-updateddate.
        ewaypartb-validupto   = i_eway-validupto.
        ewaypartb-validupto = |{ ewaypartb-validupto+8(2) }/{ ewaypartb-validupto+5(2) }/{ ewaypartb-validupto+0(4) }|.

        MODIFY zeway_part_b FROM @ewaypartb  .
      ELSE .

      ENDIF  .

    ENDIF.

    IF  w_ewaybill-ewbno  IS NOT INITIAL.
      result = |EWAY Bill No.-{ i_eway-ewbnumber } Generated .|.
    ELSEIF error_msg_part1 IS NOT INITIAL.
      result = |EWAY Bill -{ error_msg_part1 } |.
    ELSE.
      result = |EWAY Bill -{ result9 } |.
    ENDIF.
 ENDMETHOD.


  METHOD if_oo_adt_classrun~main.
  data(result) = get_table_fields( invoice = '1800000052' Eway_generate = ' '  year = '2023' companycode = '1000'  invoice1 = '18000000522024'  )  .
  ENDMETHOD.
ENDCLASS.
