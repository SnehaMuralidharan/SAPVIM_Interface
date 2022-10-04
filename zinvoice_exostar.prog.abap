

*----------------------------------------------------------------------*
*       CLASS lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_sel IMPLEMENTATION.
  METHOD m_validate_bukrs.
    IF s_bukrs[] IS NOT INITIAL.
      SELECT SINGLE @abap_true
        FROM t001
        INTO @DATA(lv_bukrs)
        WHERE bukrs IN @s_bukrs.
      IF lv_bukrs IS INITIAL.
        MESSAGE e134(zfin_msg03).
      ENDIF.
    ENDIF.

*Fetching Company Code from table
    SELECT bukrs FROM t001 INTO TABLE @DATA(lt_bukrs) WHERE bukrs IN @s_bukrs.
    LOOP AT lt_bukrs ASSIGNING FIELD-SYMBOL(<lfs_bukrs>).
*Authorization Check
      AUTHORITY-CHECK OBJECT zif_fin_global_constants_03=>gc_auth_obj2
                          ID zif_fin_global_constants_03=>zif_global_constant~gc_actvt
                       FIELD zif_fin_global_constants_03=>zif_global_constant~gc_number01
                          ID zif_fin_global_constants_03=>zif_fin_global_constants~gc_bukrs_tab
                       FIELD <lfs_bukrs>-bukrs.
      IF sy-subrc <> 0.
        DELETE s_bukrs WHERE low = <lfs_bukrs>-bukrs.
      ENDIF.
      CLEAR : <lfs_bukrs>.
    ENDLOOP.
  ENDMETHOD.

  METHOD m_validate_docid.
    IF s_docid[] IS NOT INITIAL.
      SELECT SINGLE @abap_true
        FROM /opt/vim_1head
        INTO @DATA(lv_docid)
        WHERE docid IN @s_docid.
      IF lv_docid IS INITIAL.
        MESSAGE e135(zfin_msg03).
      ENDIF.
    ENDIF.
  ENDMETHOD.
ENDCLASS.

*----------------------------------------------------------------------*
*       CLASS lcl_data IMPLEMENTATION
*----------------------------------------------------------------------*
*
*----------------------------------------------------------------------*
CLASS lcl_data IMPLEMENTATION.
  METHOD m_process_data.

    DATA: lt_values TYPE STANDARD TABLE OF zutlt_prog_ctrls.

    IF p_delta IS NOT INITIAL.

      CALL METHOD zcl_utl_hardcode_values=>get_values
        EXPORTING
          iv_reqtyp     = zif_global_constant=>gc_2ces
          iv_reqid      = zif_fin_global_constants_03=>gc_out_i5504
          iv_identifier = zif_fin_global_constants_03=>gc_out_id_i5504
          iv_name       = zif_pur_global_constants=>gc_name_last_run
          iv_counter    = zif_pur_global_constants=>gc_counter_01
          iv_sysfl      = zif_global_constant=>gc_char_cap_x
        IMPORTING
          et_values     = lt_values
        EXCEPTIONS
          not_found     = 1
          OTHERS        = 2.
      IF sy-subrc <> 0 .
*-- Last Run Timestamp is not updated in ZUTLT_PROG_CTRLS
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_129 ).
        RETURN.
      ENDIF.

      IF lt_values IS NOT INITIAL.
        DATA(ls_values) = VALUE #( lt_values[ 1 ] OPTIONAL ).
      ELSE.
*-- Last Run Timestamp is not updated in ZUTLT_PROG_CTRLS
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_129 ).
        RETURN.
      ENDIF.
    ENDIF.

    " Rejected Invoice selection from the CDS View table..
    SELECT docid,
           status,
           change_date,
           change_time,
           xblnr,
           lifnr,
           ekorg,
           atwrt,
           comment_summary
    FROM zpur_cdsv_vim_rej_inv
    INTO TABLE @DATA(lt_head)
    WHERE docid IN @s_docid
    AND bukrs IN @s_bukrs
    AND status = @zif_fin_global_constants_03=>gc_out_status
    AND xblnr NE @abap_false
    AND timestamp GT @ls_values-low.

    SORT lt_head BY docid.
    IF lt_head IS NOT INITIAL.
      LOOP AT lt_head ASSIGNING FIELD-SYMBOL(<lfs_head>).
        CLEAR: ls_proxy, lt_output-invoice_rejection-record, lv_err.
        ls_proxy-supplier_number = <lfs_head>-lifnr.
        ls_proxy-invoice_number = <lfs_head>-xblnr.
        ls_proxy-rejection_date = |{ <lfs_head>-change_date }|
        && |T| && |{ <lfs_head>-change_time TIME = ENVIRONMENT }|.
        ls_proxy-buyer_accountcode = <lfs_head>-ekorg.
        ls_proxy-mpid = <lfs_head>-atwrt.
        IF <lfs_head>-comment_summary IS NOT INITIAL.
          ls_proxy-rejection_message = <lfs_head>-comment_summary.
        ELSE.
          ls_proxy-rejection_message = TEXT-002.
        ENDIF.
        " Data is passed to the ABAP Proxy structure
        lt_output-invoice_rejection-record = ls_proxy.
        AT END OF docid.
          me->m_proxy_object( ).
        ENDAT.
      ENDLOOP.

*-- Count of Successfully Posted
      IF lv_scnt IS NOT INITIAL.
*-- No. of Invoices successfully processed: &
        lt_cnt = VALUE #( BASE lt_cnt
                     ( msgty =  zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_s
                       msgid =  zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                       msgno =  zif_fin_global_constants_03=>gc_msgno_138
                       msgv1  = CONV symsgv( lv_scnt ) ) ).
      ENDIF.

*-- Count of processed error
      IF lv_ecnt IS NOT INITIAL.
*-- No. of Invoices with processing error : &
        lt_cnt = VALUE #( BASE lt_cnt
                     ( msgty =  zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
                       msgid =  zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                       msgno =  zif_fin_global_constants_03=>gc_msgno_139
                       msgv1  = CONV symsgv( lv_ecnt ) ) ).
      ENDIF.
      " Pass information of count from LT_CNT to GT_MESSAGE
      gt_message = VALUE #( BASE gt_message ( LINES OF lt_cnt ) ).
      " Pass information of proxy log from LT_MSG to GT_MESSAGE
      gt_message = VALUE #( BASE gt_message ( LINES OF lt_msg ) ).

    ELSE.
*-- No rejected invoice(s) found since last run (timestamp) - &
      IF p_delta = abap_true.
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_131
            iv_msgv1  = ls_values-low ).
      ELSE.
*-- No rejected invoice(s) found based on the selection criteria
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_136 ).
      ENDIF.
    ENDIF.
    " Last Run is updated in the control table
    me->m_update_ctlr( ).
    CLEAR: lt_head, ls_update, ls_values, lv_scnt, lv_ecnt.
  ENDMETHOD.

  " Method for ABAP Proxy
  METHOD m_proxy_object.
    "Declaration of ABAP Proxy Class
    TRY.
        DATA(lo_proxy) = NEW zco_invoice_rejection_out_asy( ).
      CATCH cx_ai_system_fault .
*-- Error while creating invoice proxy class object
        lt_msg = VALUE #( BASE lt_msg
                     ( msgty =  zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
                       msgid =  zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                       msgno =  zif_fin_global_constants_03=>gc_msgno_128
                       msgv1  = CONV symsgv( ls_proxy-invoice_number ) ) ).
        lv_err = abap_true.
        RETURN.

    ENDTRY.
    DATA(lo_exception) = NEW cx_ai_system_fault( ).

    IF lo_proxy IS BOUND.
      TRY.
          " Data is sent to the ABAP Proxy
          CALL METHOD lo_proxy->invoice_rejection_out_asy
            EXPORTING
              output = lt_output.
          COMMIT WORK.
        CATCH cx_ai_system_fault INTO lo_exception.

          DATA(lv_error) = lo_exception->errortext.
*-- Invoice &1 : &2
          lt_msg = VALUE #( BASE lt_msg
                       ( msgty =  zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
                         msgid =  zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                         msgno =  zif_fin_global_constants_03=>gc_msgno_125
                         msgv1  = CONV symsgv( ls_proxy-invoice_number )
                         msgv2  = CONV symsgv( lv_error ) ) ).
          lv_err = abap_true.
          CLEAR: ls_proxy, lt_output-invoice_rejection-record, lv_error.
      ENDTRY.
      "Capturing log for data sent to proxy
      IF lv_err = abap_false.
        lv_scnt = lv_scnt + 1.
*-- Rejected Exostar Invoice & successfully sent to PI system
        lt_msg = VALUE #( BASE lt_msg
                     ( msgty =  zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_s
                       msgid =  zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                       msgno =  zif_fin_global_constants_03=>gc_msgno_126
                       msgv1  = CONV symsgv( ls_proxy-invoice_number ) ) ).
      ELSE.
        "Error log is updated
        lv_ecnt = lv_ecnt + 1.
      ENDIF.
    ENDIF.
  ENDMETHOD.

  " Method to update message table
  METHOD m_message.
    gt_message = VALUE #( BASE gt_message
                  ( msgty =  iv_msgtyp
                    msgid =  iv_msgid
                    msgno =  iv_msgno
                    msgv1 =  iv_msgv1
                    msgv2 =  iv_msgv2 ) ).
  ENDMETHOD.

  METHOD m_update_application_log.
    DATA: lo_applog  TYPE REF TO zcl_utl_applog.
*Call class constructor to update the application log
    IF sy-batch IS INITIAL.
      DATA(lv_display_log) = abap_true.
    ENDIF.
    CREATE OBJECT lo_applog
      EXPORTING
        iv_object        = zif_fin_global_constants_03=>gc_applog_obj_p2p_mfg
        iv_subobject     = zif_fin_global_constants_03=>gc_subobj_p2p_wbs
        iv_name          = zif_fin_global_constants_03=>gc_interface_p2p
        it_slg_msg       = gt_message
        iv_display_log   = lv_display_log
      EXCEPTIONS
        log_not_found    = 1
        msg_inconsistent = 2
        log_is_full      = 3
        OTHERS           = 4.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid  TYPE sy-msgty  NUMBER sy-msgno
                    WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ELSE.
*-- Program executed successfully.Check log (SLG1) Object-&1 / Sub-object-&2
      MESSAGE s132(zfin_msg03) WITH zif_fin_global_constants_03=>gc_applog_obj_p2p_mfg
      zif_fin_global_constants_03=>gc_subobj_p2p_wbs.
    ENDIF.
    CLEAR gt_message.
  ENDMETHOD.

  METHOD m_update_ctlr.
    DATA: lv_varkey TYPE rstable-varkey.

    lv_varkey = sy-mandt.
    " Last Run timestamp is updated in the custom table ZUTL_PROG_CTRLS
    IF p_delta = abap_true.
      CALL FUNCTION 'ENQUEUE_E_TABLE'
        EXPORTING
          mode_rstable   = zif_global_constant=>gc_char_cap_e
          tabname        = zif_fin_global_constants_03=>gc_tab_ctlr
          varkey         = lv_varkey
        EXCEPTIONS
          foreign_lock   = 1
          system_failure = 2
          OTHERS         = 3.
      IF sy-subrc = 0.
        ls_update-mandt            = sy-mandt.
        ls_update-sysid            = sy-sysid.
        ls_update-requirement_type = zif_global_constant=>gc_2ces.
        ls_update-requirement_id   = zif_fin_global_constants_03=>gc_out_i5504.
        ls_update-identifier       = zif_fin_global_constants_03=>gc_out_id_i5504.
        ls_update-counter          = zif_pur_global_constants=>gc_counter_01.
        ls_update-sign             = zif_global_constant=>gc_sign_i.
        ls_update-options          = zif_global_constant=>gc_option_eq.
        "If all Proxy calls are successful, update
        "Last Run Date and Time
        ls_update-sap_field_name   = zif_pur_global_constants=>gc_name_last_run.
        DATA(lv_timestamp)         = sy-datum && sy-uzeit.
        ls_update-low              = lv_timestamp.
        "Replacing UPDATE statement with Modify statement
        UPDATE zutlt_prog_ctrls SET low = ls_update-low
        WHERE sysid = ls_update-sysid
        AND requirement_type = ls_update-requirement_type
        AND requirement_id = ls_update-requirement_id
        AND identifier = ls_update-identifier
        AND counter = ls_update-counter
        AND sign = ls_update-sign
        AND options = ls_update-options
        AND sap_field_name = ls_update-sap_field_name.

        IF sy-subrc EQ 0.
          COMMIT WORK.
          CALL FUNCTION 'DEQUEUE_E_TABLE'
            EXPORTING
              mode_rstable = zif_global_constant=>gc_char_cap_e
              tabname      = zif_fin_global_constants_03=>gc_tab_ctlr
              varkey       = lv_varkey.
          IF sy-subrc = 0.
*-- Job run timestamp updated in table ZUTLT_PROG_CTRLS : &
            me->m_message(
              EXPORTING
                iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_s
                iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
                iv_msgno  = zif_fin_global_constants_03=>gc_msgno_137
                iv_msgv1  = CONV symsgv( lv_timestamp ) ).
          ENDIF.
        ELSE.
*-- Job run timestamp is not updated in table ZUTLT_PROG_CTRLS : &
          me->m_message(
            EXPORTING
              iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_s
              iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
              iv_msgno  = zif_fin_global_constants_03=>gc_msgno_144
              iv_msgv1  = CONV symsgv( lv_timestamp ) ).
        ENDIF.
      ELSEIF sy-subrc = 1.
*-- Last Run timestamp of Rej Inv in table ZUTLT_PROG_CTRLS is locked by &
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_141
            iv_msgv1  = sy-msgv1 ).
        ROLLBACK WORK.
      ELSE.
*-- Enqueue of Custom Table ZUTLT_PROG_CTRLS Failed
        me->m_message(
          EXPORTING
            iv_msgtyp = zif_fin_global_constants_03=>zif_global_constant~gc_symsgty_e
            iv_msgid  = zif_fin_global_constants_03=>gc_zfin_msg03 "'ZFIN_MSG03'
            iv_msgno  = zif_fin_global_constants_03=>gc_msgno_130 ).
        ROLLBACK WORK.
      ENDIF.
    ENDIF.
    CLEAR: ls_update, lv_timestamp.
  ENDMETHOD.
ENDCLASS.

**-- End of Class implementations
