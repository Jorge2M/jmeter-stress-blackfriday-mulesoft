create or replace PACKAGE BODY             CARGADATOS_MNG2 AS


  -- ----------------------------------------
  -- Descripcion: Funcion de que nos dice si
  --                 existe ya el registro en la tabla NOMBRES
  -- ----------------------------------------

  /*FUNCTION Busca_ExistNom (p_id IN VARCHAR2) RETURN NUMBER IS
i_nom NUMBER := 0;
BEGIN
  SELECT COUNT(PRODUCTO) INTO i_nom FROM NOMBRES WHERE PRODUCTO = p_id;
  IF (i_nom = 0) THEN
    RETURN 0;
  ELSE
    RETURN 1;
  END IF;

END Busca_ExistNom;*/

  -- Funcion para mirar si una tienda tiene un bloqueo de zona  en un pais
  FUNCTION existeBloqueoZonasTiendaFisica (v_pais IN VARCHAR2, v_provincia IN VARCHAR2) RETURN NUMBER IS

    i_existBloquCARGADATOS_MNG2 Body
eo NUMBER := 0;

    BEGIN
      select count(*) into i_existBloqueo from zonas_provincia_cp where zona in (select ZONA from paises_zonas where pais=v_pais and ZONA like '%DTF') and provincia =v_provincia;

      IF (i_existBloqueo = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END existeBloqueoZonasTiendaFisica;

  -- Funcion para mirar si entrega en tienda deposito esta activo en un pais
  FUNCTION entregaTdaFisicaDepoActivo (v_pais IN VARCHAR2) RETURN NUMBER IS

    i_existEntrega NUMBER := 0;

    BEGIN
      select count(*) into i_existEntrega from paises_params where pais=v_pais and param = 'shop.envio.tiendas.franquicias.allowed' and valor like '%D%';

      IF (i_existEntrega = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END entregaTdaFisicaDepoActivo;

  FUNCTION Busca_ExistNom_Idioma (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla NOMBRES
    -- ----------------------------------------

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_nom FROM NOMBRES WHERE PRODUCTO = p_producto and IDIOMA=p_idioma;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistNom_Idioma;

  FUNCTION Busca_ExistNom_Idioma_xml (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_nom FROM NOMBRES WHERE PRODUCTO = p_producto and IDIOMA=p_idioma;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistNom_Idioma_xml;

  FUNCTION Busca_descripcion_producto (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya la descripcion  en la tabla producto_descripciones
    -- ----------------------------------------
    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM producto_descripciones WHERE ID_PRODUCTO = p_producto and IDIOMA=p_idioma and linea=1;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_descripcion_producto;

  FUNCTION Busca_descripcion_corta (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion que nos dice si
    --                 existe ya la descripcion  corta en la tabla nombres
    -- ----------------------------------------

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM NOMBRES WHERE PRODUCTO = p_producto and IDIOMA=p_idioma;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_descripcion_corta;

  FUNCTION Producto_tiene_bullets (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion que nos dice si
    --                 el producto tiene la descripcion en el formato de bullets
    -- ----------------------------------------

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM PRODUCTO_SEGMENTOS WHERE ID_PRODUCTO = p_producto and IDIOMA = p_idioma;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Producto_tiene_bullets;

  FUNCTION Busca_desc_producto (p_producto IN VARCHAR2, p_idioma IN VARCHAR2) RETURN VARCHAR2 IS
    -- ----------------------------------------
    -- Descripcion: Funcion que nos devuelve 3l nombre
    --------------------------------------------

    nombre  varchar2(2000):='';

    BEGIN
      SELECT descripcion INTO nombre FROM nombres WHERE PRODUCTO = p_producto and IDIOMA=p_idioma;

      return nombre;

      EXCEPTION WHEN OTHERS THEN
      return nombre;

    END Busca_desc_producto;
  -- ----------------------------------------
  -- Descripcion: Procedimiento de carga
  --              de datos de Nombres.
  -- ----------------------------------------

  /*PROCEDURE Carga_Nombres(p_prd IN VARCHAR2,p_es IN VARCHAR2,p_in IN VARCHAR2,p_fr IN VARCHAR2,p_al IN VARCHAR2, p_ca IN VARCHAR2, p_tr IN VARCHAR2) IS
v_existe NUMBER;
BEGIN
 v_existe := Busca_ExistNom(p_prd);
 IF (v_existe = 0) THEN
     --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'ES',REPLACE(REPLACE(RTRIM(p_es),CHR(39),''),CHR(128),'&Ccedil;'));
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'IN',REPLACE(REPLACE(RTRIM(p_in),CHR(39),''),CHR(128),'&Ccedil;'));
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'FR',REPLACE(REPLACE(RTRIM(p_fr),CHR(39),''),CHR(128),'&Ccedil;'));
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'AL',REPLACE(REPLACE(RTRIM(p_al),CHR(39),''),CHR(128),'&Ccedil;'));
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'CA',REPLACE(REPLACE(RTRIM(p_ca),CHR(39),''),CHR(128),'&Ccedil;'));
     INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'ES',REPLACE(REPLACE(REPLACE(RTRIM(p_es),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'IN',REPLACE(REPLACE(REPLACE(RTRIM(p_in),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'FR',REPLACE(REPLACE(REPLACE(RTRIM(p_fr),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'AL',REPLACE(REPLACE(REPLACE(RTRIM(p_al),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'CA',REPLACE(REPLACE(REPLACE(RTRIM(p_ca),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'TR',REPLACE(REPLACE(REPLACE(RTRIM(p_ca),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'ES',p_es);
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'IN',p_in);
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'FR',p_fr);
    --INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,'AL',p_al);
 ELSE
     --UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(RTRIM(p_es),CHR(39),''),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='ES';
    --UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(RTRIM(p_in),CHR(39),''),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='IN';
    --UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(RTRIM(p_fr),CHR(39),''),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='FR';
    --UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(RTRIM(p_al),CHR(39),''),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='AL';
    --UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(RTRIM(p_ca),CHR(39),''),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='CA';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_es),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='ES';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_in),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='IN';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_fr),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='FR';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_al),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='AL';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_ca),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='CA';
    UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_tr),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_prd AND IDIOMA='TR';
    --UPDATE NOMBRES SET DESCRIPCION=p_es WHERE PRODUCTO=p_prd AND IDIOMA='ES';
    --UPDATE NOMBRES SET DESCRIPCION=p_in WHERE PRODUCTO=p_prd AND IDIOMA='IN';
    --UPDATE NOMBRES SET DESCRIPCION=p_fr WHERE PRODUCTO=p_prd AND IDIOMA='FR';
    --UPDATE NOMBRES SET DESCRIPCION=p_al WHERE PRODUCTO=p_prd AND IDIOMA='AL';

 END IF;
END Carga_Nombres;*/


  PROCEDURE Carga_Nombres_Idioma(p_producto IN VARCHAR2,p_idioma IN VARCHAR2,p_descripcion IN NVARCHAR2) IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Nombres por Idioma.
    -- ----------------------------------------

    v_existe NUMBER;

    BEGIN
      v_existe := Busca_ExistNom_Idioma(p_producto, p_idioma);

      IF (v_existe = 0) THEN
        INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_producto,p_idioma,REPLACE(REPLACE(REPLACE(RTRIM(p_descripcion),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
      ELSE
        UPDATE NOMBRES SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(p_descripcion),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') WHERE PRODUCTO=p_producto AND IDIOMA=p_idioma;
      END IF;
    END Carga_Nombres_Idioma;


  FUNCTION Busca_ExistComp_Idioma (p_id IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla COMPOSICIONES e Idioma
    -- ----------------------------------------

    i_comp NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_comp FROM COMPOSICIONES WHERE PRODUCTO = p_id and IDIOMA=p_idioma;

      IF (i_comp = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistComp_Idioma;


  FUNCTION Busca_ExistComp_Idioma_xml (p_id IN VARCHAR2, p_idioma IN VARCHAR2) RETURN NUMBER IS

    i_comp NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_comp FROM COMPOSICIONES WHERE PRODUCTO = p_id and IDIOMA=p_idioma;

      IF (i_comp = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistComp_Idioma_xml;


  PROCEDURE Carga_Composiciones_Idioma(p_prd IN VARCHAR2,p_idioma IN VARCHAR2,p_composicion IN NVARCHAR2) IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Composiciones por Idioma.
    -- ----------------------------------------

    v_existe NUMBER;

    BEGIN
      v_existe := Busca_ExistComp_Idioma(p_prd,p_idioma);

      IF (v_existe = 0) THEN
        INSERT INTO COMPOSICIONES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (p_prd,p_idioma,p_composicion);
      ELSE
        UPDATE COMPOSICIONES SET DESCRIPCION=p_composicion WHERE PRODUCTO=p_prd AND IDIOMA=p_idioma;
      END IF;

    END Carga_Composiciones_Idioma;

  -- ----------------------------------------
  -- Descripcion: Funcion de que nos dice si
  --                 existe ya el registro en la tabla REBAJAS
  -- ----------------------------------------

  /*FUNCTION Busca_ExistReb (p_id IN VARCHAR2,p_tipo IN NUMBER) RETURN NUMBER IS
i_reb NUMBER :=0 ;
BEGIN
  SELECT COUNT(PRODUCTO) INTO i_reb FROM REBAJAS WHERE PRODUCTO = p_id AND TIPO = p_tipo;
  IF (i_reb = 0) THEN
    RETURN 0;
  ELSE
    RETURN 1;
  END IF;
END Busca_ExistReb;*/

  -- ----------------------------------------
  -- Descripcion: Procedimiento de carga
  --              de datos de Composiciones.
  -- ----------------------------------------

  /*PROCEDURE Carga_Rebajas(p_prd IN VARCHAR2,p_pvp IN VARCHAR2,p_tipo IN NUMBER) IS
v_existe NUMBER;
BEGIN
 v_existe := Busca_ExistReb(p_prd,p_tipo);
 IF (v_existe = 0) THEN
    INSERT INTO REBAJAS(PRODUCTO,PRECIO,TIPO) VALUES (p_prd,translate(p_pvp,',','.'),p_tipo);
 ELSE
    UPDATE REBAJAS SET PRECIO=translate(p_pvp,',','.') WHERE PRODUCTO=p_prd AND TIPO=p_tipo;
 END IF;
END Carga_Rebajas;*/


  FUNCTION Busca_ExistPrd (p_id IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla PRODUCTOS
    -- ----------------------------------------
    i_prd NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_prd FROM PRODUCTOS WHERE ID = p_id;

      IF (i_prd = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrd;

  FUNCTION Busca_ExistPrd_xml (p_id IN VARCHAR2) RETURN NUMBER IS

    i_prd NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_prd FROM PRODUCTOS WHERE ID = p_id;

      IF (i_prd = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrd_xml;

  FUNCTION Busca_ExistArt_xml (p_id IN VARCHAR2, p_color IN VARCHAR2) RETURN NUMBER IS

    i_prd NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_prd FROM ARTICULOS WHERE PRODUCTO = p_id AND COLOR = p_color;

      IF (i_prd = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistArt_xml;

  FUNCTION Busca_ExistPrdAlmacen (p_id IN VARCHAR2, p_almacen IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla PRODUCTOS_ALMACEN
    -- ----------------------------------------
    i_prd NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_prd FROM PRODUCTO_ALMACEN WHERE PRODUCTO = p_id and ALMACEN = p_almacen;

      IF (i_prd = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrdAlmacen;

  FUNCTION Busca_ExistPrdAlmacen_xml (p_id IN VARCHAR2, p_almacen IN VARCHAR2) RETURN NUMBER IS

    i_prd NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_prd FROM PRODUCTO_ALMACEN WHERE PRODUCTO = p_id and ALMACEN = p_almacen;

      IF (i_prd = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrdAlmacen_xml;

  FUNCTION Busca_ExistArticulosAlmacen (p_id IN VARCHAR2, p_almacen IN VARCHAR2) RETURN NUMBER IS

    i_art NUMBER := 0;

    BEGIN
      SELECT COUNT(id) INTO i_art FROM ARTICULOS_ALMACEN WHERE id = p_id and ALMACEN = p_almacen;

      IF (i_art = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistArticulosAlmacen;

  FUNCTION Busca_GTalla (p_id IN VARCHAR2) RETURN NUMBER IS

    i_gtalla NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_gtalla FROM REL_PRODUCTO_GTALLA WHERE PRODUCTO = p_id;

      IF (i_gtalla = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_GTalla;

  FUNCTION Busca_GTalla_xml (p_id IN VARCHAR2) RETURN NUMBER IS

    i_gtalla NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_gtalla FROM REL_PRODUCTO_GTALLA WHERE PRODUCTO = p_id;

      IF (i_gtalla = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_GTalla_xml;

  FUNCTION Busca_Tendencia (p_id IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- JJCF
    -- Descripcion: Obtiene identificador de tendencia
    --
    -- ----------------------------------------
    BEGIN
      -- FOR R IN (SELECT ID FROM TENDENCIAS WHERE DESCRIPCION  = p_id) LOOP
      --  RETURN (R.ID);
      -- EXIT;
      -- END LOOP;
      RETURN 0;
    END Busca_Tendencia;



  PROCEDURE Carga_Productos IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Productos.
    -- ----------------------------------------
    v_tendencia NUMBER;
    v_existe  NUMBER;
    v_gtalla NUMBER;
    v_fecha DATE;
    v_comp_es VARCHAR2(2000);
    v_comp_in VARCHAR2(2000);
    v_comp_fr VARCHAR2(2000);
    v_comp_al VARCHAR2(2000);
    v_comp_ca VARCHAR2(2000);
    v_comp_tu VARCHAR2(2000);
    v_traza VARCHAR2(2000) := '';

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PRODUCTOS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PRODUCTOS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;

    num_inserts             number;
    num_updates             number;
    num_inserts2            number;
    num_updates2            number;
    num_inserts3            number;
    num_updates3            number;

    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      num_inserts := 0;
      num_updates := 0;
      num_inserts2 := 0;
      num_updates2 := 0;
      num_inserts3 := 0;
      num_updates3 := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PRODUCTOS');

      FOR c_prd IN
      (SELECT * FROM MNGDES2.MNG_PRODUCTOS ORDER BY fecha ASC)
      LOOP
        num_regs := num_regs + 1;

        -- Carga de la tabla NOMBRES
        v_traza := 'Carga de la tabla NOMBRES';
        Carga_Nombres_Idioma(c_prd.MODELO,'ES',c_prd.DESCRIPCION_ES);
        Carga_Nombres_Idioma(c_prd.MODELO,'IN',c_prd.DESCRIPCION_IN);
        Carga_Nombres_Idioma(c_prd.MODELO,'FR',c_prd.DESCRIPCION_FR);
        Carga_Nombres_Idioma(c_prd.MODELO,'AL',c_prd.DESCRIPCION_AL);
        Carga_Nombres_Idioma(c_prd.MODELO,'CA',c_prd.DESCRIPCION_CA);
        Carga_Nombres_Idioma(c_prd.MODELO,'TU',c_prd.DESCRIPCION_TU);

        -- Carga de la tabla COMPOSICIONES
        v_traza := 'Carga de la tabla COMPOSICIONES';
        v_comp_es:= c_prd.COMPOSICION_ES1||'<BR>'||c_prd.COMPOSICION_ES2||'<BR>'||c_prd.COMPOSICION_ES3||'<BR>'||c_prd.COMPOSICION_ES4;
        v_comp_in:= c_prd.COMPOSICION_IN1||'<BR>'||c_prd.COMPOSICION_IN2||'<BR>'||c_prd.COMPOSICION_IN3||'<BR>'||c_prd.COMPOSICION_IN4;
        v_comp_fr:= c_prd.COMPOSICION_FR1||'<BR>'||c_prd.COMPOSICION_FR2||'<BR>'||c_prd.COMPOSICION_FR3||'<BR>'||c_prd.COMPOSICION_FR4;
        v_comp_al:= c_prd.COMPOSICION_AL1||'<BR>'||c_prd.COMPOSICION_AL2||'<BR>'||c_prd.COMPOSICION_AL3||'<BR>'||c_prd.COMPOSICION_AL4;
        v_comp_ca:= c_prd.COMPOSICION_CA1||'<BR>'||c_prd.COMPOSICION_CA2||'<BR>'||c_prd.COMPOSICION_CA3||'<BR>'||c_prd.COMPOSICION_CA4;
        v_comp_tu:= c_prd.COMPOSICION_TU1||'<BR>'||c_prd.COMPOSICION_TU2||'<BR>'||c_prd.COMPOSICION_TU3||'<BR>'||c_prd.COMPOSICION_TU4;

        Carga_Composiciones_Idioma(c_prd.MODELO,'ES',v_comp_es);
        Carga_Composiciones_Idioma(c_prd.MODELO,'IN',v_comp_in);
        Carga_Composiciones_Idioma(c_prd.MODELO,'FR',v_comp_fr);
        Carga_Composiciones_Idioma(c_prd.MODELO,'AL',v_comp_al);
        Carga_Composiciones_Idioma(c_prd.MODELO,'CA',v_comp_ca);
        Carga_Composiciones_Idioma(c_prd.MODELO,'TU',v_comp_tu);

        -- Carga de la tabla REBAJAS

        /*v_traza := 'Carga de la tabla REBAJAS';
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS,1);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS_2,2);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_SALDO,3);*/

        v_existe := Busca_ExistPrd(c_prd.MODELO);
        --v_tendencia := Busca_Tendencia(c_prd.TENDENCIAS);

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          IF (c_prd.ALMACEN = '001') THEN
            num_inserts := num_inserts + 1;

            INSERT INTO PRODUCTOS
            (ID,
             REF_FABRICA,
             GENERICO,
             PVP,
             ESTILO,
             FAMILIA,
             TENDENCIA,
             FECHA_BAJA_001,
             CODIGO_DISENYO,
             TIPO_ARTICULO,
             GENERO,
             BASE)
            VALUES
              (c_prd.MODELO,
                TRIM(c_prd.MODELO_FABRICA),
                NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                translate(c_prd.PRECIO,',','.'),
                c_prd.LINEA,
                c_prd.FAMILIA,
                c_prd.TENDENCIAS,
                v_fecha,
                c_prd.CODIGO_DISENYO,
                c_prd.TIPO_ARTICULO,
                c_prd.GENERO,
               c_prd.BASE);
          ELSE

            IF (c_prd.ALMACEN = '400') THEN
              num_inserts := num_inserts + 1;

              INSERT INTO PRODUCTOS
              (ID,
               REF_FABRICA,
               GENERICO,
               PVP,
               ESTILO,
               FAMILIA,
               TENDENCIA,
               FECHA_BAJA_400,
               CODIGO_DISENYO,
               TIPO_ARTICULO,
               GENERO,
               BASE)
              VALUES
                (c_prd.MODELO,
                  TRIM(c_prd.MODELO_FABRICA),
                  NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                  translate(c_prd.PRECIO,',','.'),
                  c_prd.LINEA,
                  c_prd.FAMILIA,
                  c_prd.TENDENCIAS,
                  v_fecha,
                  c_prd.CODIGO_DISENYO,
                  c_prd.TIPO_ARTICULO,
                  c_prd.GENERO,
                 c_prd.BASE);

            ELSE

              IF (c_prd.ALMACEN = '052') THEN
                num_inserts := num_inserts + 1;

                INSERT INTO PRODUCTOS
                (ID,
                 REF_FABRICA,
                 GENERICO,
                 PVP,
                 ESTILO,
                 FAMILIA,
                 TENDENCIA,
                 CODIGO_DISENYO,
                 TIPO_ARTICULO,
                 GENERO,
                 BASE)
                VALUES
                  (c_prd.MODELO,
                    TRIM(c_prd.MODELO_FABRICA),
                    NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                    translate(c_prd.PRECIO,',','.'),
                    c_prd.LINEA,
                    c_prd.FAMILIA,
                    c_prd.TENDENCIAS,
                    c_prd.CODIGO_DISENYO,
                    c_prd.TIPO_ARTICULO,
                    c_prd.GENERO,
                    c_prd.BASE);
              ELSE
                IF(c_prd.ALMACEN = '004') THEN
                  num_inserts := num_inserts + 1;

                  INSERT INTO PRODUCTOS
                  (ID,
                   REF_FABRICA,
                   GENERICO,
                   PVP,
                   ESTILO,
                   FAMILIA,
                   TENDENCIA,
                   CODIGO_DISENYO,
                   TIPO_ARTICULO,
                   GENERO,
                   BASE)
                  VALUES
                    (c_prd.MODELO,
                      TRIM(c_prd.MODELO_FABRICA),
                      NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                      translate(c_prd.PRECIO,',','.'),
                      c_prd.LINEA,
                      c_prd.FAMILIA,
                      c_prd.TENDENCIAS,
                      c_prd.CODIGO_DISENYO,
                      c_prd.TIPO_ARTICULO,
                      c_prd.GENERO,
                      c_prd.BASE);
                END IF;
              END IF;
            END IF;
          END IF;
        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            IF (c_prd.ALMACEN = '001') THEN
              num_updates := num_updates + 1;

              UPDATE PRODUCTOS SET    REF_FABRICA = TRIM(c_prd.MODELO_FABRICA),
                GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                PVP = translate(c_prd.PRECIO,',','.'),
                ESTILO = c_prd.LINEA,
                FAMILIA = c_prd.FAMILIA,
                TENDENCIA = c_prd.TENDENCIAS,
                FECHA_BAJA_001 = v_fecha,
                CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
                TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
                GENERO = c_prd.GENERO,
                BASE = c_prd.BASE
              WHERE ID = c_prd.MODELO;

            ELSE
              IF (c_prd.ALMACEN = '400') THEN
                num_updates := num_updates + 1;

                UPDATE PRODUCTOS SET    REF_FABRICA =TRIM(c_prd.MODELO_FABRICA),
                  GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                  PVP = translate(c_prd.PRECIO,',','.'),
                  ESTILO = c_prd.LINEA,
                  FAMILIA = c_prd.FAMILIA,
                  TENDENCIA = c_prd.TENDENCIAS,
                  FECHA_BAJA_400 = v_fecha,
                  CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
                  TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
                  GENERO = c_prd.GENERO,
                  BASE = c_prd.BASE
                WHERE ID = c_prd.MODELO;

              ELSE

                IF (c_prd.ALMACEN = '052') THEN
                  num_updates := num_updates + 1;

                  UPDATE PRODUCTOS SET    REF_FABRICA =TRIM(c_prd.MODELO_FABRICA),
                    GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                    PVP = translate(c_prd.PRECIO,',','.'),
                    ESTILO = c_prd.LINEA,
                    FAMILIA = c_prd.FAMILIA,
                    TENDENCIA = c_prd.TENDENCIAS,
                    CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
                    TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
                    GENERO = c_prd.GENERO,
                    BASE = c_prd.BASE
                  WHERE ID = c_prd.MODELO;
                ELSE
                  IF (c_prd.ALMACEN = '004') THEN
                    num_updates := num_updates + 1;

                    UPDATE PRODUCTOS SET    REF_FABRICA =TRIM(c_prd.MODELO_FABRICA),
                      GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
                      PVP = translate(c_prd.PRECIO,',','.'),
                      ESTILO = c_prd.LINEA,
                      FAMILIA = c_prd.FAMILIA,
                      TENDENCIA = c_prd.TENDENCIAS,
                      CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
                      TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
                      GENERO = c_prd.GENERO,
                      BASE = c_prd.BASE
                    WHERE ID = c_prd.MODELO;
                  END IF;
                END IF;
              END IF;
            END IF;
          ELSE
            IF (c_prd.ALMACEN = '001') THEN
              num_updates := num_updates + 1;

              UPDATE PRODUCTOS SET FECHA_BAJA_001 = SYSDATE
              WHERE ID = c_prd.MODELO;

            ELSE

              IF (c_prd.ALMACEN = '400') THEN
                num_updates := num_updates + 1;

                UPDATE PRODUCTOS SET FECHA_BAJA_400 = SYSDATE
                WHERE ID = c_prd.MODELO;

              END IF;

            END IF;
          END IF;
        END IF;

        v_existe := Busca_ExistPrdAlmacen(c_prd.MODELO, c_prd.ALMACEN);

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          num_inserts2 := num_inserts2 + 1;

          INSERT INTO PRODUCTO_ALMACEN(PRODUCTO,ALMACEN,FECHA_BAJA) VALUES (c_prd.MODELO,c_prd.ALMACEN,v_fecha);

        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            num_updates2 := num_updates2 + 1;

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = v_fecha
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN= c_prd.ALMACEN;

          ELSE
            num_updates2 := num_updates2 + 1;

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = SYSDATE
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN= c_prd.ALMACEN;

          END IF;

        END IF;

        v_gtalla := Busca_GTalla(c_prd.MODELO);

        IF  (v_gtalla = 0) THEN
          -- insertar un nuevo producto en la tabla de grupos de tallas
          INSERT INTO REL_PRODUCTO_GTALLA
          (PRODUCTO, GRUPO_TALLA)
          VALUES
            (c_prd.MODELO, c_prd.GRUPO_TALLAS);

          num_inserts3 := num_inserts3 + 1;
        ELSE
          -- Actualizar la tabla de greupos de tallas
          UPDATE REL_PRODUCTO_GTALLA
          SET GRUPO_TALLA=c_prd.GRUPO_TALLAS
          WHERE PRODUCTO=c_prd.MODELO;

          num_updates3 := num_updates3 + 1;
        END IF;

      END LOOP;

      -- KIKU 20/1/2014 Sols faig un commit!
      COMMIT;


      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      status_exe_proc := status_exe_proc || ' (I:' ||to_char(num_inserts)||' U:'||to_char(num_updates)||
                         ' I2:' ||to_char(num_inserts2)||' U2:'||to_char(num_updates2)||
                         ' I3:' ||to_char(num_inserts3)||' U3:'||to_char(num_updates3)||')';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      --ACT_TMP; -- ACTUALIZA BBDD TMP_ARTICULOS
      --ACT_TMP_PRECIOS;

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(status_exe_proc);
    END Carga_Productos;

  PROCEDURE load_product_desc IS
    -- -------------------------------------
    -- Descripción: Procedimiento para cargar las descripciones/composiciones  de los productos
    -- Autor: Daniel Rodríguez
    -----------------------------------------
    v_existe  NUMBER;
    v_tiene_bullets NUMBER;
    err_code varchar2(200):='';
    err_msg varchar2(200):='';
    v_comp varchar2(2000):='';
    v_nombre varchar2(2000):='';
    v_descripcion_corta varchar2(2000):='';
    txt nvarchar2(2000):='';
    existe_nom varchar2(2000):= '';
    existe_desc varchar2(2000):='';
    moreno number;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'LOAD_PRODUCT_DESC';
    sub_id_proc             CONSTANT varchar2(30) := 'LOAD_PRODUCT_DESC';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;

    num_inserts             number;
    num_updates             number;
    num_updatess             number;
    num_inserts2            number;
    num_updates2            number;
    num_updates2s           number;
    num_inserts3            number;
    num_updates3            number;
    num_updates3s           number;
    num_inserts4            number;
    num_updates4            number;
    num_updates4s           number;
    num_updates5s           number;
    num_updates5            number;

    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      num_inserts := 0;
      num_updatess := 0;
      num_updates := 0;
      num_inserts2 := 0;
      num_updates2 := 0;
      num_updates2s := 0;
      num_inserts3 := 0;
      num_updates3 := 0;
      num_updates3s := 0;
      num_inserts4 := 0;
      num_updates4 := 0;
      num_updates4s := 0;
      num_updates5 := 0;
      num_updates5s := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant PRODUCTO_DESC_IDI');

  -- *KIKU* 30-10-2018 En comptes d'anar registre a registre a mirar tot de taules per si existeixen.. faig que ho resolgui la select d'una tacada ;)
  --                   Amb 38000 registres, passem de 22segons a 15
      FOR c_prd IN
        (
            SELECT distinct a.*, nvl2(n.producto,1,0) as v_existe_nom, nvl2(ps.id_producto,1,0) as v_tiene_bullets, nvl2(c.producto,1,0) as v_existe_comp, nvl2(d.id_producto,1,0) as v_existe_desc 
              FROM MNGDES2.producto_desc_idi a left join NOMBRES n on n.producto = a.modelo and n.idioma = a.idioma 
                                               left join PRODUCTO_SEGMENTOS ps on ps.ID_PRODUCTO = a.modelo and ps.IDIOMA = a.idioma
                                               left join COMPOSICIONES c on c.producto = a.modelo and c.idioma = a.idioma
                                               left join PRODUCTO_DESCRIPCIONES d on d.id_producto = a.modelo and d.idioma = a.idioma
             ORDER BY a.fecha ASC, a.modelo
        )
      LOOP
        num_regs := num_regs + 1;
        existe_nom := c_prd.v_existe_nom;
        existe_desc := c_prd.v_existe_desc;

        IF(LENGTH(C_PRD.nombre)>0 AND C_PRD.nombre != 'NA')    THEN
          --v_existe := Busca_ExistNom_Idioma_xml(c_prd.modelo, c_prd.idioma);

          txt:= c_prd.nombre;
          
          IF (existe_nom = 0) THEN
            INSERT INTO NOMBRES(PRODUCTO,IDIOMA,DESCRIPCION) VALUES (c_prd.modelo,c_prd.idioma,REPLACE(REPLACE(REPLACE(RTRIM(txt),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;'));
            num_inserts := num_inserts + 1;
            existe_nom := 1;
          ELSE
            num_updatess := num_updatess + 1;
            
            -- *KIKU* 26/7/2021 Sols farà el update dels que hagin canviat realment!!
            UPDATE NOMBRES 
               SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(txt),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') 
             WHERE PRODUCTO=c_prd.modelo AND IDIOMA=c_prd.idioma AND
                   (DESCRIPCION <> REPLACE(REPLACE(REPLACE(RTRIM(txt),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') or DESCRIPCION is null);
                     
            num_updates := num_updates + sql%rowcount;
          END IF;
        END IF;

        -- *KIKU* 26/01/2015 Millores codificació (menys CPU)
        --                   Abans feia: replace(replace(upper(substr(lower(c_prd.COMPOSICION_1),0,1))||substr(lower(c_prd.COMPOSICION_1),2),'%','% '),':',': ')
        --                   i ara farà: replace(replace(upper(substr(c_prd.COMPOSICION_1,0,1))||lower(substr(c_prd.COMPOSICION_1,2)),'%','% '),':',': ')
        --v_existe := Busca_ExistComp_Idioma_xml(c_prd.modelo,c_prd.idioma);
        IF (c_prd.v_existe_comp = 0) THEN
          INSERT INTO COMPOSICIONES(PRODUCTO,IDIOMA,DESCRIPCION)
          VALUES (c_prd.modelo,c_prd.idioma,
                  replace(replace(upper(substr(c_prd.COMPOSICION_1,0,1))||lower(substr(c_prd.COMPOSICION_1,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_2,0,1))||lower(substr(c_prd.COMPOSICION_2,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_3,0,1))||lower(substr(c_prd.COMPOSICION_3,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_4,0,1))||lower(substr(c_prd.COMPOSICION_4,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_5,0,1))||lower(substr(c_prd.COMPOSICION_5,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_6,0,1))||lower(substr(c_prd.COMPOSICION_6,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_7,0,1))||lower(substr(c_prd.COMPOSICION_7,2)),'%','% '),':',': ')||'<br>'||
                  replace(replace(upper(substr(c_prd.COMPOSICION_8,0,1))||lower(substr(c_prd.COMPOSICION_8,2)),'%','% '),':',': '));

          num_inserts2 := num_inserts2 + 1;
        ELSE
          num_updates2s := num_updates2s + 1;
          
          -- *KIKU* 26/7/2021 Sols farà el update dels que hagin canviat realment!!
          UPDATE COMPOSICIONES
             SET DESCRIPCION=
              replace(replace(upper(substr(c_prd.COMPOSICION_1,0,1))||lower(substr(c_prd.COMPOSICION_1,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_2,0,1))||lower(substr(c_prd.COMPOSICION_2,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_3,0,1))||lower(substr(c_prd.COMPOSICION_3,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_4,0,1))||lower(substr(c_prd.COMPOSICION_4,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_5,0,1))||lower(substr(c_prd.COMPOSICION_5,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_6,0,1))||lower(substr(c_prd.COMPOSICION_6,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_7,0,1))||lower(substr(c_prd.COMPOSICION_7,2)),'%','% '),':',': ')||'<br>'||
              replace(replace(upper(substr(c_prd.COMPOSICION_8,0,1))||lower(substr(c_prd.COMPOSICION_8,2)),'%','% '),':',': ')
          WHERE PRODUCTO=c_prd.modelo AND IDIOMA=c_prd.idioma AND
                (DESCRIPCION <> replace(replace(upper(substr(c_prd.COMPOSICION_1,0,1))||lower(substr(c_prd.COMPOSICION_1,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_2,0,1))||lower(substr(c_prd.COMPOSICION_2,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_3,0,1))||lower(substr(c_prd.COMPOSICION_3,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_4,0,1))||lower(substr(c_prd.COMPOSICION_4,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_5,0,1))||lower(substr(c_prd.COMPOSICION_5,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_6,0,1))||lower(substr(c_prd.COMPOSICION_6,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_7,0,1))||lower(substr(c_prd.COMPOSICION_7,2)),'%','% '),':',': ')||'<br>'||
                               replace(replace(upper(substr(c_prd.COMPOSICION_8,0,1))||lower(substr(c_prd.COMPOSICION_8,2)),'%','% '),':',': ')
                 OR DESCRIPCION is null);
 
          num_updates2 := num_updates2 + sql%rowcount;
        END IF;

        -- Comprobamos si en producto ya tiene informadas las descripciones por el nuevo gestor de descripciones
        --v_tiene_bullets := Producto_tiene_bullets(c_prd.modelo, c_prd.idioma);

        IF(c_prd.v_tiene_bullets = 0) THEN
          IF(LENGTH(C_PRD.descripcion_larga)>0 AND C_PRD.descripcion_larga != 'NA')    THEN
            --v_existe := Busca_descripcion_producto(c_prd.modelo,c_prd.idioma);

            IF (existe_desc = 0) THEN
              INSERT INTO PRODUCTO_DESCRIPCIONES(ID_PRODUCTO,IDIOMA,LINEA,TEXTO,ESTILO) VALUES (c_prd.modelo,c_prd.idioma,1,replace(replace(C_PRD.descripcion_larga,'&lt;','<'),'&gt;','>'),'N');
              existe_desc := 1;

              num_inserts3 := num_inserts3 + 1;
            ELSE
              num_updates3s := num_updates3s + 1;
              
              -- *KIKU* 26/7/2021 Sols farà el update dels que hagin canviat realment!!
              UPDATE PRODUCTO_DESCRIPCIONES 
                 SET TEXTO=replace(replace(C_PRD.descripcion_larga,'&lt;','<'),'&gt;','>') 
               WHERE ID_PRODUCTO=c_prd.modelo AND IDIOMA=c_prd.idioma AND LINEA=1 AND
                     (TEXTO <> replace(replace(C_PRD.descripcion_larga,'&lt;','<'),'&gt;','>') or TEXTO is null);

              num_updates3 := num_updates3 + sql%rowcount;
            END IF;
          END IF;

          IF(LENGTH(C_PRD.descripcion_corta)>0)    THEN
            --v_existe := Busca_descripcion_corta(c_prd.modelo,c_prd.idioma);

            IF (existe_nom = 0) THEN
              v_descripcion_corta:=c_prd.modelo;
              INSERT INTO nombres(PRODUCTO,IDIOMA,DESCRIPCION_CORTA) VALUES (c_prd.modelo,c_prd.idioma,c_prd.descripcion_corta);

              num_inserts4 := num_inserts4 + 1;
              existe_nom := 1;
            ELSE
              num_updates4s := num_updates4s + 1;

              -- *KIKU* 26/7/2021 Sols farà el update dels que hagin canviat realment!!
              UPDATE nombres 
                 SET DESCRIPCION_CORTA=c_prd.descripcion_corta
               WHERE PRODUCTO=c_prd.modelo AND IDIOMA=c_prd.idioma 
                 AND (DESCRIPCION_CORTA <> c_prd.descripcion_corta or DESCRIPCION_CORTA is null);
                                    
              num_updates4 := num_updates4 + sql%rowcount;
            END IF;
          END IF;
        END IF;

      END LOOP;

      -- PARCHE  QUE SALGAN LOS NOMBRES DE LOS PRODUCTOS EN INGLES, CUANDO EL IDIOMA ES JAPO
      FOR c_prd_JP IN
      (SELECT * FROM MNGDES2.producto_desc_idi WHERE idioma='JA' ORDER BY fecha ASC)
      LOOP
        v_existe := Busca_ExistNom_Idioma_xml(c_prd_JP.modelo,'IN');

        IF (v_existe>0)  THEN
          v_nombre:=Busca_desc_producto(c_prd_JP.modelo,'IN');

          num_updates5s := num_updates5s + 1;

          -- *KIKU* 26/7/2021 Sols farà el update dels que hagin canviat realment!!
          UPDATE NOMBRES  
             SET DESCRIPCION=REPLACE(REPLACE(REPLACE(RTRIM(v_nombre),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') 
           WHERE PRODUCTO=c_prd_JP.modelo AND IDIOMA=c_prd_JP.idioma AND
                 (DESCRIPCION <> REPLACE(REPLACE(REPLACE(RTRIM(v_nombre),CHR(39),''),CHR(165),'&Ntilde;'),CHR(128),'&Ccedil;') or DESCRIPCION is null); 

          num_updates5 := num_updates5 + sql%rowcount;
        END IF;
      END LOOP;

      --Parche para los productos de Touch que tienen que salir en Mango. Se añade a la descripcion_corta el literal TOUCH.
      --update nombres tmp set descripcion_corta = 'TOUCH - '||descripcion_corta
      --where tmp.producto in (select id from productos where genero = 'T') and substr(descripcion_corta,1,5) <> 'TOUCH'
      --and descripcion_corta is not null;
      --COMMIT;

      --
      -- Se quitan las modificaciones sobre PRODUCTO_DESCRIPCIONES.TEXTO ya que ahora se gestionan con el Gestor de descripciones
      --

      --Parche para los productos de SPORTS que quieren poner una descripcion fija en el inicio de su descripcion:
      update nombres set descripcion_corta = 'Yoga - '||descripcion_corta
      where producto in (select distinct id_producto from SPORTS_NAMES where valor = 'Yoga') and descripcion_corta is not null and (substr(descripcion_corta,0,7)!='Yoga - ') and substr(descripcion_corta,0,7)!='Fitness';

      update nombres set descripcion_corta = 'Fitness '||'&'||' Running - '||descripcion_corta
      where producto in (select distinct id_producto from SPORTS_NAMES where valor = 'Running') and descripcion_corta is not null and (substr(descripcion_corta,0,7)!='Fitness');

      --RedMine 30025
      update nombres set descripcion_corta = 'ONLINE EXCLUSIVE - '||descripcion_corta
      where producto in (select distinct id from promociones where promocion = '672') and descripcion_corta is not null and (substr(descripcion_corta,0,19)!='ONLINE EXCLUSIVE - ');

      --RedMine 30027/30975
      update nombres set descripcion_corta = 'COUNTRY SPECIALS - '||descripcion_corta
      where producto in (select distinct id from promociones where promocion = '673') and descripcion_corta is not null and (substr(descripcion_corta,0,19)!='COUNTRY SPECIALS - ');

      --RedMine 30997
      update nombres set descripcion_corta = 'SPECIAL SIZES - '||descripcion_corta
      where producto in (select distinct id from promociones where promocion = '13') and descripcion_corta is not null and (substr(descripcion_corta,0,16)!='SPECIAL SIZES - ');

      -- SOPO-3700 Eliminar la palabra tencel de la composicion (06/07/2017)
      update composiciones set descripcion = replace(upper(descripcion),'TENCEL®/LYOCELL','lyocell') where upper(descripcion) like '%TENCEL%';
      update composiciones set descripcion = replace(upper(descripcion),'TENCEL','lyocell') where upper(descripcion) like '%TENCEL%';

      --Redmine 31159 (No ha de sortir en lloc la paraula TENCEL)
      --update nombres set descripcion = replace(descripcion,'TENCEL','') where upper(descripcion) like '%TENCEL%';
      --update nombres set descripcion_corta = replace(descripcion_corta,'tencelu','') where upper(descripcion_corta) like '%TENCELU%';
      --update nombres set descripcion_corta = replace(descripcion_corta,'tencel','') where upper(descripcion_corta) like '%TENCEL%';
      --update nombres set descripcion_corta = replace(descripcion_corta,'TENCEL','') where upper(descripcion_corta) like '%TENCEL%';
      --update producto_descripciones set texto = replace(texto,'tencelu','') where upper(texto) like '%TENCELU%';
      --update producto_descripciones set texto = replace(texto,'tencel','') where upper(texto) like '%TENCEL%';
      --update producto_descripciones set texto = replace(texto,'Tencel','') where upper(texto) like '%TENCEL%';

      -- KIKU 20/1/2014 Sols farà un commit
      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      status_exe_proc := status_exe_proc || ' (I:' ||to_char(num_inserts)||' Us:'||to_char(num_updatess)||' U:'||to_char(num_updates)||
                         ' I2:' ||to_char(num_inserts2)||' U2s:'||to_char(num_updates2s)||' U2:'||to_char(num_updates2)||
                         ' I3:' ||to_char(num_inserts3)||' U3s:'||to_char(num_updates3s)||' U3:'||to_char(num_updates3)||
                         ' I4:' ||to_char(num_inserts4)||' U4s:'||to_char(num_updates4s)||' U4:'||to_char(num_updates4)||
                                                         ' U5s:'||to_char(num_updates5s)||' U5:'||to_char(num_updates5)||
                         ')';
      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');


      EXCEPTION
      WHEN OTHERS THEN
      
      err_code := SQLCODE;
      err_msg := substr(SQLERRM, 1, 200);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;
           
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(SQLERRM);

      anotar_traza (err_code||' : '||err_msg,'CARGADATOS_MNG".load_product_desc',60);
      anotar_traza (v_descripcion_corta,'CARGADATOS_MNG".load_product_desc',60);

    END load_product_desc;

  PROCEDURE load_bloqueos_votf IS
    err_code varchar2(200):='';
    err_msg varchar2(200):='';
    v_existe  NUMBER;


    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'LOAD_BLOQUEOS_VOTF';
    sub_id_proc             CONSTANT varchar2(30) := 'LOAD_BLOQUEOS_VOTF';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant BLOQUEOS_TIENDAS_VOTF_TMP');

      FOR c_bloqueo IN
      (SELECT * FROM MNGDES2.BLOQUEOS_TIENDAS_VOTF_TMP )
      LOOP
        num_regs := num_regs + 1;

        UPDATE TIENDAS_FISICAS
        SET bloqueo_votf=c_bloqueo.bloqueo
        WHERE ID = c_bloqueo.id;
      END LOOP;

      -- KIKU 17/1/2014: Sols farà un commit al final
      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      err_code := SQLCODE;
      err_msg := substr(SQLERRM, 1, 200);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(SQLERRM);

      anotar_traza(err_code||' : '||err_msg,'CARAGADATOS_MNG2.LOAD_BLOQUEOS_VOTF',60);

    END load_bloqueos_votf;

  FUNCTION existeTiendaFisicaGenero (v_id IN VARCHAR2) RETURN NUMBER IS

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM MNGDES2.GENEROS_TIENDAS_TMP WHERE ID = v_id;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END existeTiendaFisicaGenero;

  FUNCTION existeGenero (v_id IN VARCHAR2, v_genero IN VARCHAR2, v_temporada IN VARCHAR2) RETURN NUMBER IS

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM MNGDES2.GENEROS_TIENDAS_TMP WHERE ID=v_id AND genero=v_genero AND temporada=v_temporada;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END existeGenero;

  PROCEDURE load_genero_tiendas IS
    err_code varchar2(200):='';
    err_msg varchar2(200):='';
    v_temporada varchar2(1);
    tienda_activa varchar2(1):='';
    v_existe  NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'LOAD_GENERO_TIENDAS';
    sub_id_proc             CONSTANT varchar2(30) := 'LOAD_GENERO_TIENDAS';
    -- Altres Variables
    permis_execucio         char(1);

    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant TIENDAS_FISICAS');

      select valor into v_temporada from ATRIBUTOS_MANGO WHERE ATRIBUTO='TEMPORADA_ACTIVA';

      FOR c_tienda IN
      (SELECT id FROM TIENDAS_FISICAS)
      LOOP
        num_regs := num_regs + 1;

        v_existe := existeTiendaFisicaGenero(c_tienda.id);

        if (v_existe=1) then
          delete from tiendas_fisicas_tipo_tiendas
          where tienda = c_tienda.ID and
                tipo in (1,2,4,5,6,7,8); --borramos todos excepto TOUCH. En el load_tiendas_fisicas, actualizamos si tiene TOUCH

          --Tratamos SHE
          v_existe := existeGenero(c_tienda.id,'M',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='M' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,1);
            end if;
          end if;

          --Tratamos  HE
          v_existe := existeGenero(c_tienda.id,'H',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='H' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,2);
            end if;
          end if;

          --Tratamos KIDS NIÑO
          v_existe := existeGenero(c_tienda.id,'O',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='O' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,4);
            end if;
          end if;

          --Tratamos KIDS NIÑA
          v_existe := existeGenero(c_tienda.id,'A',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='A' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,5);
            end if;
          end if;

          --Tratamos SPORTS
          v_existe := existeGenero(c_tienda.id,'S',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='S' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,6);
            end if;
          end if;

          --Tratamos INTIMATE
          v_existe := existeGenero(c_tienda.id,'I',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='I' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,7);
            end if;
          end if;

          --Tratamos VIOLETA
          v_existe := existeGenero(c_tienda.id,'V',v_temporada);

          if(v_existe=1) then
            select activo into tienda_activa
            from MNGDES2.GENEROS_TIENDAS_TMP
            where id=c_tienda.id and genero='V' and temporada=v_temporada;

            if(tienda_activa='S') then
              insert into tiendas_fisicas_tipo_tiendas values(c_tienda.id,8);
            end if;
          end if;

          tienda_activa:='';
        end if;

      END LOOP;

      -- KIKU 20/1/2014 Sols faig un commit
      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      err_code := SQLCODE;
      err_msg := substr(SQLERRM, 1, 200);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(SQLERRM);

      anotar_traza(err_code||' : '||err_msg,'CARAGADATOS_MNG2.LOAD_GENERO_TIENDA',60);

    END load_genero_tiendas;


  FUNCTION existeTiendaFisica (v_id IN VARCHAR2) RETURN NUMBER IS
    -- -------------------------------------
    -- Descripción: Procedimiento para cargar las tiendas fisicas
    -- Autor: Daniel Rodríguez
    -----------------------------------------

    i_nom NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_nom FROM TIENDAS_FISICAS WHERE ID = v_id;

      IF (i_nom = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END existeTiendaFisica;

  PROCEDURE load_tiendas_fisicas IS
    err_code varchar2(200):='';
    err_msg varchar2(200):='';
    v_existe  NUMBER;
    v_genero varchar(2) :='';
    num_new number;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'LOAD_TIENDAS_FISICAS';
    sub_id_proc             CONSTANT varchar2(30) := 'LOAD_TIENDAS_FISICAS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;
      num_new := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant TIENDAS_FISICAS_TMP');

      FOR c_tienda IN
      (SELECT * FROM MNGDES2.tiendas_fisicas_tmp)
      LOOP
        num_regs := num_regs + 1;

        v_existe := existeTiendaFisica(c_tienda.CAMPO1);

        IF (v_existe = 0) THEN
          insert into tiendas_fisicas(ID,NOMBRE_TIENDA,ESTADO_TIENDA,TIPO_CLIENTE,CODIGO_PAIS,TIPO_CENTRO,OUTLET,GESTION_DEPOSITO,GESTION_PROPIA,FECHA_CIERRE,FECHA_APERTURA,TIPO_TIENDA,FECHA_APERTURA_INICIAL, FECHA_CIERRE_DEFINITIVO,AEROPUERTO,FECHA_CIERRE_PREVISTA,ES_TIENDA,UBICACION_TIENDA,EMAIL)
          values (c_tienda.CAMPO1,c_tienda.CAMPO2,c_tienda.CAMPO3,c_tienda.CAMPO14,c_tienda.CAMPO25,c_tienda.CAMPO40,c_tienda.CAMPO18,c_tienda.CAMPO19,c_tienda.CAMPO79,c_tienda.CAMPO83,c_tienda.CAMPO84,c_tienda.CAMPO53||c_tienda.CAMPO59,c_tienda.CAMPO5,c_tienda.CAMPO7,c_tienda.CAMPO35,c_tienda.CAMPO6,c_tienda.CAMPO39,c_tienda.CAMPO32,TRIM(c_tienda.CAMPO198));

          num_new := num_new + 1;

          -- *KIKU* 28/7/14 Aquest insert no te sentit ja que tot seguit l'esborra!!
          -- insert into tiendas_fisicas_tipo_tiendas values(c_tienda.CAMPO1,1); --Añadimos la tienda Mango por defecto
        ELSE
          UPDATE tiendas_fisicas
          SET ESTADO_TIENDA=c_tienda.CAMPO3,
            FECHA_CIERRE=c_tienda.CAMPO83,
            FECHA_APERTURA=c_tienda.CAMPO84,
            TIPO_CLIENTE=c_tienda.CAMPO14,
            OUTLET=c_tienda.CAMPO18,
            GESTION_DEPOSITO=c_tienda.CAMPO19,
            GESTION_PROPIA=c_tienda.CAMPO79,
            TIPO_TIENDA=c_tienda.CAMPO53||c_tienda.CAMPO59,
            ACTIVA_WINTEGRATE='S',
            FECHA_APERTURA_INICIAL=c_tienda.CAMPO5,
            FECHA_CIERRE_DEFINITIVO=c_tienda.CAMPO7,
            AEROPUERTO=c_tienda.CAMPO35,
            NOMBRE_TIENDA=c_tienda.CAMPO2,
            FECHA_CIERRE_PREVISTA=c_tienda.CAMPO6,
            CODIGO_PAIS=c_tienda.CAMPO25,
            ES_TIENDA=c_tienda.CAMPO39,
            UBICACION_TIENDA=c_tienda.CAMPO32,
            EMAIL=TRIM(c_tienda.CAMPO198)
          WHERE ID= c_tienda.CAMPO1;
        END IF;

        -- Logica para dar de baja algunas tiendas (Depositos) y que no salgan en la web
        IF ( entregaTdaFisicaDepoActivo(c_tienda.CAMPO25) = 1 and c_tienda.CAMPO14 = 'D') THEN
          -- Tiendas con limitaciones por zona
          IF ( existeBloqueoZonasTiendaFisica(c_tienda.CAMPO25,c_tienda.CAMPO23) = 1 ) THEN
            -- Deshabilitamos la TIENDA POR zona
            UPDATE tiendas_fisicas SET BAJA_SHOP='S', BAJA_SHOP_EMPLEADO='S', DESCRIPCION_BAJA='TIENDA EN ZONA BLOQUEADA' WHERE ID= c_tienda.CAMPO1;
          END IF;


        END IF;


        --Eliminamos los generos de cada tienda
        DELETE FROM tiendas_fisicas_tipo_tiendas WHERE tienda= c_tienda.CAMPO1;

        --Miramos si vende mango
        v_genero := substr(c_tienda.CAMPO143,1,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,1); --Añadimos la tienda she

          MERGE INTO paises_marcas_tiendas_fisicas pmtf USING (SELECT id_tienda FROM generos_tiendas where id_genero_num = 1) marca
          ON(pmtf.id_pais = c_tienda.CAMPO25 AND pmtf.id_shop = marca.id_tienda)
          WHEN NOT MATCHED THEN INSERT (ID_PAIS, ID_SHOP) VALUES (c_tienda.CAMPO25, marca.id_tienda);
        END IF;

        --Miramos si vende he
        v_genero := substr(c_tienda.CAMPO143,2,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,2); --Añadimos la tienda he

          MERGE INTO paises_marcas_tiendas_fisicas pmtf USING (SELECT id_tienda FROM generos_tiendas where id_genero_num = 2) marca
          ON(pmtf.id_pais = c_tienda.CAMPO25 AND pmtf.id_shop = marca.id_tienda)
          WHEN NOT MATCHED THEN INSERT (ID_PAIS, ID_SHOP) VALUES (c_tienda.CAMPO25, marca.id_tienda);
        END IF;

        --Miramos si vende touch
        v_genero := substr(c_tienda.CAMPO143,3,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,3); --Añadimos la tienda Touch
        END IF;

        --Miramos si vende kids
        v_genero := substr(c_tienda.CAMPO143,4,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,4); --Añadimos la tienda KIDS
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,5); --Añadimos la tienda KIDS

          MERGE INTO paises_marcas_tiendas_fisicas pmtf USING (SELECT id_tienda FROM generos_tiendas where id_genero_num = 4) marca
          ON(pmtf.id_pais = c_tienda.CAMPO25 AND pmtf.id_shop = marca.id_tienda)
          WHEN NOT MATCHED THEN INSERT (ID_PAIS, ID_SHOP) VALUES (c_tienda.CAMPO25, marca.id_tienda);
        END IF;

        --Miramos si vende sports
        v_genero := substr(c_tienda.CAMPO143,5,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,6); --Añadimos la tienda sports
        END IF;

        --Miramos si vende intimate
        v_genero := substr(c_tienda.CAMPO143,6,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,7); --Añadimos la tienda intimate
        END IF;

        --Miramos si vende violeta
        v_genero := substr(c_tienda.CAMPO143,8,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,8); --Añadimos la tienda violeta

          MERGE INTO paises_marcas_tiendas_fisicas pmtf USING (SELECT id_tienda FROM generos_tiendas where id_genero_num = 8) marca
          ON(pmtf.id_pais = c_tienda.CAMPO25 AND pmtf.id_shop = marca.id_tienda)
          WHEN NOT MATCHED THEN INSERT (ID_PAIS, ID_SHOP) VALUES (c_tienda.CAMPO25, marca.id_tienda);
        END IF;

        --Miramos si vende Baby
        v_genero := substr(c_tienda.CAMPO143,9,1);
        IF( v_genero = 'S' ) THEN
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,9); --Añadimos la tienda BABY
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,10); --Añadimos la tienda BABY
          INSERT INTO tiendas_fisicas_tipo_tiendas VALUES(c_tienda.CAMPO1,11); --Añadimos la tienda BABY
        END IF;

        -- Si viene informado el usuario de VOTF en el fichero
        IF (c_tienda.CAMPO196 IS NOT NULL) THEN

          -- Creamos el registro en la tabla de usuarios y paswords de venta asistida como bloqueado por carga inicial
          MERGE INTO USUARIOS_VENTA_ASISTIDA uva USING DUAL
          ON (uva.USERNAME = LOWER(c_tienda.CAMPO196))
          WHEN NOT MATCHED THEN INSERT (USERNAME, DESCRIPCION, BLOQUEADO, TIPO_DE_USUARIO) VALUES (LOWER(c_tienda.CAMPO196), 'Creacion automatica al crear nueva tienda', 'S', 'V');

          -- Creamos la asociacion de la tienda fisica con el usuario si no existe
          MERGE INTO USUARIOS_VENTA_ASIS_TDA_FISICA uvatf USING DUAL
          ON (uvatf.USERNAME = TRIM(LOWER(c_tienda.CAMPO196)))
          WHEN NOT MATCHED THEN INSERT (USERNAME, ID_TIENDA, MASTER) VALUES (LOWER(c_tienda.CAMPO196), TRIM(c_tienda.CAMPO1), 'N')
          WHEN MATCHED THEN UPDATE SET uvatf.ID_TIENDA = TRIM(c_tienda.CAMPO1);

        END IF;

      END LOOP;

      -- *KIKU* 28/7/14 Aquest Update estava dins del bucle i no te cap sentit! El trec fora del bucle pq s'executi sols un cop!
      -- PARCHE para 2 tiendas que se encuentran en CUBA, pero están inscritas en España. Son 2 excepciones de Mango.
      UPDATE tiendas_fisicas
      SET codigo_pais='448'
      WHERE id in ('00004236','00004343');

      -- KIKU 20/1/2014 Sols farà un commit
      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres - ' ||
                         to_char(num_new)||' Noves';
      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      err_code := SQLCODE;
      err_msg := substr(SQLERRM, 1, 200);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(SQLERRM);

      anotar_traza(err_code||' : '||err_msg,'CARAGADATOS_MNG2.LOAD_TIENDA_FISICAS',60);

    END load_tiendas_fisicas;

  PROCEDURE load_product_info IS
    -- -------------------------------------
    -- Descripción: Procedimiento para cargar los atributos comunes de los productos
    -- Autor: Daniel Rodríguez
    -----------------------------------------
    v_tendencia NUMBER;

    v_existe  NUMBER;
    v_gtalla NUMBER;

    v_fecha DATE;
    v_comp_cn NVARCHAR2(2000);
    v_traza VARCHAR2(2000) := '';

    err_code varchar2(200):='';
    err_msg varchar2(200):='';

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'LOAD_PRODUCT_INFO';
    sub_id_proc             CONSTANT varchar2(30) := 'LOAD_PRODUCT_INFO';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;

    num_inserts             number;
    num_updates             number;
    num_inserts2            number;
    num_updates2            number;
    num_inserts3            number;
    num_updates3            number;

    num_del_nl              number;
    num_ins_nl              number;
    num_del_tnl             number;
    num_ins_tnl             number;


    -- id articulo
    v_id_articulo  varchar2(12);
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      num_inserts := 0;
      num_updates := 0;
      num_inserts2 := 0;
      num_updates2 := 0;
      num_inserts3 := 0;
      num_updates3 := 0;

      num_del_nl := 0;
      num_ins_nl := 0;
      num_del_tnl := 0;
      num_ins_tnl := 0;

      -- Per si de cas hi ha un altre càrrega executant-se al mateix moment
      execute immediate ('ALTER SESSION SET ddl_lock_timeout=180');
      
      -- *KIKU* 21/01/2015 En comptes de fer n mil deletes... faig un delete previ de tots el que venen a la taula a carregar
      --PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Delete PRODUCTO_NORMA_LAVADO');
      --DELETE FROM producto_norma_lavado where producto in (select distinct modelo from MNGDES2.productos_attr);

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant PRODUCTOS_ATTR');

      FOR c_prd IN
      (SELECT * FROM MNGDES2.productos_attr ORDER BY fecha ASC)
      LOOP
        num_regs := num_regs + 1;

        v_existe := Busca_ExistPrd_xml(c_prd.MODELO);

        IF (v_existe = 0) THEN
          num_inserts := num_inserts + 1;

          INSERT INTO PRODUCTOS
          (ID,
           REF_FABRICA,
           GENERICO,
           PVP,
           ESTILO,
           FAMILIA,
           TENDENCIA,
           CODIGO_DISENYO,
           TIPO_ARTICULO,
           GENERO,
           BASE,
           FAMILIA_A_SEQ,
           FAMILIA_B_SEQ,
           TEMPORADA,
           FECHA_EMBARQUE,
           PAIS_ORIGEN,
           COLECCION,
           PARTIDA_ARANCELARIA)
          VALUES
            (c_prd.MODELO,
              TRIM(c_prd.MODELO_FABRICA),
              NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
              0,
              nvl(c_prd.LINEA,'C'),
              substr(c_prd.FAMILIA_A,2,1),
              c_prd.TENDENCIAS,
              c_prd.CODIGO_DISENO,
              c_prd.TIPO_ARTICULO,
              c_prd.TIPO_GENERO,
              c_prd.BASE,
             substr(c_prd.familia_a,2),
             c_prd.familia_b,
             substr(c_prd.MODELO,0,1),
             DECODE(is_date(c_prd.fecha_embarque),'T', to_date(c_prd.fecha_embarque , 'YYYYMMDD'),null ),
             LPAD(c_prd.pais_origen,3,'0'),
             c_prd.TEMPORADA,
             c_prd.PARTIDA_ARANCELARIA);
        ELSE  --Entra aqui si el producto existe
          num_updates := num_updates + 1;

          UPDATE PRODUCTOS SET    REF_FABRICA = TRIM(c_prd.MODELO_FABRICA),
            GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
            PVP = 0,
            ESTILO = nvl(c_prd.LINEA,'C'),
            TENDENCIA = c_prd.TENDENCIAS,
            CODIGO_DISENYO = c_prd.CODIGO_DISENO,
            FAMILIA=substr(c_prd.FAMILIA_A,2,1),
            TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
            GENERO = c_prd.TIPO_GENERO,
            BASE = c_prd.BASE,
            familia_a_seq = substr(c_prd.familia_a,2),
            familia_b_seq = c_prd.familia_b,
            temporada = substr(c_prd.MODELO,0,1),
            fecha_embarque = DECODE(is_date(c_prd.fecha_embarque),'T', to_date(c_prd.fecha_embarque , 'YYYYMMDD'),null ),
            pais_origen = LPAD(c_prd.pais_origen,3,'0'),
            coleccion = c_prd.TEMPORADA,
            PARTIDA_ARANCELARIA = c_prd.PARTIDA_ARANCELARIA
          WHERE ID =  c_prd.MODELO;
        END IF;

        -- Colores
        FOR c_prd_articulos IN (SELECT * FROM MNGDES2.articulos_attr where modelo = c_prd.MODELO)
        LOOP
          v_existe := Busca_ExistArt_xml(c_prd_articulos.MODELO, c_prd_articulos.COLOR);

          IF (v_existe != 0) THEN

            UPDATE ARTICULOS SET COSTE = c_prd_articulos.COSTE,
              PRIORIDAD = c_prd_articulos.PRIORIDAD,
              CODIGO_NRF = NVL2(c_prd_articulos.NRF,LPAD(c_prd_articulos.NRF,3,'0'),ARTICULOS.CODIGO_NRF)
            WHERE ID like c_prd_articulos.MODELO||'%'||c_prd_articulos.COLOR;
          END IF;

        END LOOP;
        -- Fin Colores


        -- Articulos

        FOR c_articulos IN (SELECT modelo, lpad(talla, 2, '0') talla, color, peso, bloque FROM MNGDES2.articulos where modelo = c_prd.MODELO)
        LOOP

          v_id_articulo  := c_articulos.MODELO || c_articulos.TALLA || c_articulos.COLOR;

          -- actualizamos tabla articulos
          -- Si no existe lo creamos y si existe solo actualizamos peso y bloque
          MERGE INTO ARTICULOS A USING (SELECT v_id_articulo as ID, c_articulos.modelo as PRODUCTO, c_articulos.talla as TALLA, c_articulos.color as COLOR, c_articulos.peso as PESO_BRUTO, c_articulos.bloque as BLOQUE, c_prd.grupo_tallas as GRUPO_TALLA  FROM DUAL) B
          ON (A.PRODUCTO = B.PRODUCTO and A.TALLA = B.TALLA and A.COLOR = B.COLOR)
          WHEN NOT MATCHED THEN INSERT ( ID, PRODUCTO, TALLA, COLOR, PESO_BRUTO, BLOQUE, GRUPO_TALLA) VALUES (B.ID, B.PRODUCTO, B.TALLA, B.COLOR, B.PESO_BRUTO, B.BLOQUE, B.GRUPO_TALLA)
          WHEN MATCHED THEN UPDATE SET  A.PESO_BRUTO = B.PESO_BRUTO, A.BLOQUE = B.BLOQUE, A.GRUPO_TALLA = B.GRUPO_TALLA;

          -- actualizamos tabla articulos almacen si no existe con stock 0
          v_existe := Busca_ExistArticulosAlmacen(v_id_articulo,c_prd.almacen);

          IF (v_existe = 0) THEN

            INSERT INTO ARTICULOS_ALMACEN ( ID, ALMACEN, FECHA_BAJA, STOCK_REAL, STOCK_MINIMO, STOCK_IDEAL, FECHA_ACT)
            VALUES (c_articulos.MODELO || c_articulos.TALLA || c_articulos.COLOR, c_prd.almacen, sysdate, 0, 0, 0, sysdate);

          END IF;

        END LOOP;

        -- Fin Articulos

        v_existe := Busca_ExistPrdAlmacen_xml(c_prd.MODELO,c_prd.almacen);

        --Ignoramos de momento el campo de la fecha de baja, porque no nos deberia venir ningun producto de baja en los PROPAL
        --Informatica esta mirando la incidencia
        IF (v_existe = 0) THEN

          -- IF (c_prd.ESTADO = 'B') THEN
          --         v_fecha := sysdate;
          -- ELSE
          --       v_fecha := '';
          -- END IF;

          num_inserts2 := num_inserts2 + 1;

          INSERT INTO PRODUCTO_ALMACEN
          (PRODUCTO,
           ALMACEN,
           FECHA_BAJA)
          VALUES
            (c_prd.MODELO,
             c_prd.almacen,
             '');

        ELSE  --Entra aqui si el producto existe
          --IF (c_prd.ESTADO = 'B') THEN
          --    v_fecha :=  sysdate;
          --ELSE
          --    v_fecha := '';
          --END IF;

          num_updates2 := num_updates2 + 1;

          UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = ''
          WHERE PRODUCTO = c_prd.MODELO AND ALMACEN=c_prd.almacen;
        END IF;


        v_gtalla := Busca_GTalla_xml(c_prd.MODELO);

        IF  (v_gtalla = 0) THEN
          num_inserts3 := num_inserts3 + 1;

          -- insertar un nuevo producto en la tabla de grupos de tallas
          INSERT INTO REL_PRODUCTO_GTALLA
          (PRODUCTO, GRUPO_TALLA)
          VALUES
            (c_prd.MODELO, c_prd.GRUPO_TALLAS);
        ELSE
          num_updates3 := num_updates3 + 1;

          -- Actualizar la tabla de greupos de tallas
          UPDATE REL_PRODUCTO_GTALLA
          SET GRUPO_TALLA=c_prd.GRUPO_TALLAS
          WHERE PRODUCTO=c_prd.MODELO;
        END IF;

        --Actualizamos los simbolos de lavado
        DELETE FROM producto_norma_lavado where producto=c_prd.MODELO;
        num_del_nl := num_del_nl + sql%rowcount;

        IF(C_prd.SIMBOLO_LAVADO_1 is not null) THEN
          INSERT INTO producto_norma_lavado(PRODUCTO,NORMA_LAVADO) VALUES (c_prd.MODELO, decode(c_prd.SIMBOLO_LAVADO_1,'09','9','04','4','01','1','02','2','03','3',c_prd.SIMBOLO_LAVADO_1) );
          num_ins_nl := num_ins_nl + 1;
        end if;

        IF(C_prd.SIMBOLO_LAVADO_2 is not  null) THEN
          INSERT INTO producto_norma_lavado(PRODUCTO,NORMA_LAVADO) VALUES (c_prd.MODELO, decode(c_prd.SIMBOLO_LAVADO_2,'09','9','04','4','01','1','02','2','03','3',c_prd.SIMBOLO_LAVADO_2) );
          num_ins_nl := num_ins_nl + 1;
        END IF;

        IF(C_prd.SIMBOLO_LAVADO_3 is not  null) THEN
          INSERT INTO producto_norma_lavado(PRODUCTO,NORMA_LAVADO) VALUES (c_prd.MODELO, decode(c_prd.SIMBOLO_LAVADO_3,'09','9','04','4','01','1','02','2','03','3',c_prd.SIMBOLO_LAVADO_3) );
          num_ins_nl := num_ins_nl + 1;
        END IF;

        IF(C_prd.SIMBOLO_LAVADO_4 is not  null) THEN
          INSERT INTO producto_norma_lavado(PRODUCTO,NORMA_LAVADO) VALUES (c_prd.MODELO, decode(c_prd.SIMBOLO_LAVADO_4,'09','9','04','4','01','1','02','2','03','3',c_prd.SIMBOLO_LAVADO_4) );
          num_ins_nl := num_ins_nl + 1;
        END IF;

        IF(C_prd.SIMBOLO_LAVADO_5 is not  null) THEN
          INSERT INTO producto_norma_lavado(PRODUCTO,NORMA_LAVADO) VALUES (c_prd.MODELO, decode(c_prd.SIMBOLO_LAVADO_5,'09','9','04','4','01','1','02','2','03','3',c_prd.SIMBOLO_LAVADO_5) );
          num_ins_nl := num_ins_nl + 1;
        END IF;


-- *KIKU* 20/7/2021 En comptes d'esborrar tot un producte un a un i després fer l'insert de tot.. ho converteixo a un DELETE+MERGE un cop executat el bucle
--
--        -- *KIKU* 22/6/2015 Actualitzo taula temporal per millorar select NORMAS_LAVADO (Redmine #23576)
--        BEGIN
--          DELETE FROM TMP_NORMAS_LAVADO where PRODUCTO = c_prd.MODELO;
--          num_del_tnl := num_del_tnl + sql%rowcount;
--
--          EXCEPTION
--          WHEN NO_DATA_FOUND THEN
--          NULL;
--        END;
--
--        BEGIN
--          INSERT INTO TMP_NORMAS_LAVADO
--            SELECT p.producto, n.idioma, to_number(n.id) as id, n.descripcion
--            FROM normas_lavado n
--              INNER JOIN producto_norma_lavado p ON p.norma_lavado = n.id
--            where P.PRODUCTO = c_prd.MODELO;
--          num_ins_tnl := num_ins_tnl + sql%rowcount;
--
--          EXCEPTION
--          WHEN NO_DATA_FOUND THEN
--          NULL;
--        END;
      END LOOP;
      
      -- *KIKU* 20/7/2021 L'actualització de la taula TMP_NORMAS_LAVADO es fa aquí d'un sol cop i no pas producte a producte
      
      -- Pas #1. Esborro els registres que ja no existeixen (el merge sols fa insert/update) 
      delete from tmp_normas_lavado where(producto, idioma, id) not in 
      (
            SELECT p.producto, n.idioma, to_number(n.id) as id
            FROM normas_lavado n
              INNER JOIN producto_norma_lavado p ON p.norma_lavado = n.id
            where P.PRODUCTO in (select modelo from MNGDES2.productos_attr)
      ) and producto in (select modelo from MNGDES2.productos_attr);
      
      num_del_tnl := sql%rowcount;

      -- Pas #2. Actualitzo sols els registres amb canvis!!
      MERGE INTO tmp_normas_lavado t1
      USING 
      (
        with sel as
        (
                SELECT p.producto, n.idioma, to_number(n.id) as id, n.descripcion
                FROM normas_lavado n
                  INNER JOIN producto_norma_lavado p ON p.norma_lavado = n.id
                where P.PRODUCTO in (select modelo from MNGDES2.productos_attr)
        ) 
        SELECT * from sel
        MINUS 
        (
            select * from sel
            INTERSECT
            SELECT * FROM tmp_normas_lavado
        )
      ) t
        ON (t.producto = t1.producto AND t.idioma = t1.idioma and t.id = t1.id)             
      WHEN NOT MATCHED THEN
        INSERT 
            (PRODUCTO, IDIOMA, ID, DESCRIPCION)
        VALUES
            (t.PRODUCTO, t.IDIOMA, t.ID, t.DESCRIPCION)
      WHEN MATCHED THEN
        UPDATE 
        SET DESCRIPCION = t.DESCRIPCION;

      num_ins_tnl := sql%rowcount;

      -- KIKU 20/1/2014 Sols faig un commit
      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      status_exe_proc := status_exe_proc || ' (I:' ||to_char(num_inserts)||' U:'||to_char(num_updates)||
                         ' I2:' ||to_char(num_inserts2)||' U2:'||to_char(num_updates2)||
                         ' I3:' ||to_char(num_inserts3)||' U3:'||to_char(num_updates3)||')';
      status_exe_proc := status_exe_proc || ' NL(D:' ||to_char(num_del_nl)||' I:'||to_char(num_ins_nl)||')'||
                         ' TNL(D:' ||to_char(num_del_tnl)||' I:'||to_char(num_ins_tnl)||')';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      --ACT_TMP; -- ACTUALIZA BBDD TMP_ARTICULOS
      --ACT_TMP_PRECIOS;

      EXCEPTION
      WHEN OTHERS THEN

      err_code := SQLCODE;
      err_msg := substr(SQLERRM, 1, 200);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);


    END load_product_info;

  PROCEDURE Carga_Productos_Chi IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Productos.
    -- ----------------------------------------
    v_tendencia NUMBER;
    v_existe  NUMBER;
    v_gtalla NUMBER;

    v_fecha DATE;
    v_comp_cn NVARCHAR2(2000);
    v_traza VARCHAR2(2000) := '';

    BEGIN

      FOR c_prd IN
      (SELECT * FROM MNGDES2.MNG_PRODUCTOS_CHI ORDER BY fecha ASC)
      LOOP
        -- Carga de la tabla NOMBRES
        v_traza := 'Carga de la tabla NOMBRES';
        Carga_Nombres_Idioma(c_prd.MODELO,'ZH',c_prd.DESCRIPCION );
        -- Carga de la tabla COMPOSICIONES
        v_traza := 'Carga de la tabla COMPOSICIONES';
        v_comp_cn:= c_prd.COMPOSICION1||'<BR>'||c_prd.COMPOSICION2||'<BR>'||c_prd.COMPOSICION3||'<BR>'||c_prd.COMPOSICION4;
        Carga_Composiciones_Idioma(c_prd.MODELO,'ZH',v_comp_cn);
        -- Carga de la tabla REBAJAS

        commit;

        /*v_traza := 'Carga de la tabla REBAJAS';
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS,1);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS_2,2);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_SALDO,3);*/

        v_existe := Busca_ExistPrd(c_prd.MODELO);
        --v_tendencia := Busca_Tendencia(c_prd.TENDENCIAS);

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          INSERT INTO PRODUCTOS
          (ID,
           REF_FABRICA,
           GENERICO,
           PVP,
           ESTILO,
           --FAMILIA,
           TENDENCIA,
           CODIGO_DISENYO,
           TIPO_ARTICULO,
           GENERO,
           BASE)
          VALUES
            (c_prd.MODELO,
             TRIM(c_prd.MODELO_FABRICA),
             NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
             translate(c_prd.PRECIO,',','.'),
             c_prd.LINEA,
             --c_prd.FAMILIA,
             c_prd.TENDENCIAS,
             c_prd.CODIGO_DISENYO,
             c_prd.TIPO_ARTICULO,
             c_prd.GENERO,
             c_prd.BASE);

          --insert into traza_jordi values('a001'||c_prd.MODELO,sysdate);

          COMMIT;

        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            UPDATE PRODUCTOS SET    REF_FABRICA = TRIM(c_prd.MODELO_FABRICA),
              GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
              PVP = translate(c_prd.PRECIO,',','.'),
              ESTILO = c_prd.LINEA,
              --FAMILIA = c_prd.FAMILIA,
              TENDENCIA = c_prd.TENDENCIAS,
              CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
              TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
              GENERO = c_prd.GENERO,
              BASE = c_prd.BASE
            WHERE ID = c_prd.MODELO;

            --insert into traza_jordi values('b001'||c_prd.MODELO,sysdate);

            COMMIT;

          END IF;

        END IF;

        v_existe := Busca_ExistPrdAlmacen(c_prd.MODELO, '720');--Forzamos alamacen China

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          INSERT INTO PRODUCTO_ALMACEN
          (PRODUCTO,
           ALMACEN,
           FECHA_BAJA)
          VALUES
            (c_prd.MODELO,
             '720',
             v_fecha);

          --insert into traza_jordi values('a001'||c_prd.MODELO,sysdate);


          COMMIT;

        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = v_fecha
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN='720';

            --insert into traza_jordi values('b001'||c_prd.MODELO,sysdate);

            COMMIT;

          ELSE

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = SYSDATE
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN='720';

            COMMIT;

          END IF;

        END IF;

        v_gtalla := Busca_GTalla(c_prd.MODELO);

        IF  (v_gtalla = 0) THEN
          -- insertar un nuevo producto en la tabla de grupos de tallas
          INSERT INTO REL_PRODUCTO_GTALLA
          (PRODUCTO, GRUPO_TALLA)
          VALUES
            (c_prd.MODELO, c_prd.GRUPO_TALLAS);
          COMMIT;
        ELSE
          -- Actualizar la tabla de greupos de tallas
          UPDATE REL_PRODUCTO_GTALLA
          SET GRUPO_TALLA=c_prd.GRUPO_TALLAS
          WHERE PRODUCTO=c_prd.MODELO;
          COMMIT;
        END IF;

      END LOOP;

      --ACT_TMP; -- ACTUALIZA BBDD TMP_ARTICULOS
      --ACT_TMP_PRECIOS;

      exception when others then

      DBMS_OUTPUT.PUT_LINE(SQLERRM);
      --insert into traza_jordi values('error en insercion de productos',sysdate);
    END Carga_Productos_Chi;


  PROCEDURE Carga_Productos_XML IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Productos.
    -- ----------------------------------------
    v_tendencia NUMBER;
    v_existe  NUMBER;
    v_gtalla NUMBER;

    v_fecha DATE;
    v_comp_cn NVARCHAR2(2000);
    v_traza VARCHAR2(2000) := '';

    BEGIN

      FOR c_prd IN
      (SELECT * FROM MNGDES2.MNG_PRODUCTOS_XML ORDER BY fecha_act ASC)
      LOOP
        -- Carga de la tabla NOMBRES
        v_traza := 'Carga de la tabla NOMBRES';
        Carga_Nombres_Idioma(c_prd.MODELO,'RU',c_prd.DESCRIPCION );
        -- Carga de la tabla COMPOSICIONES
        v_traza := 'Carga de la tabla COMPOSICIONES';
        v_comp_cn:= c_prd.COMPOSICION1||'<BR>'||c_prd.COMPOSICION2||'<BR>'||c_prd.COMPOSICION3||'<BR>'||c_prd.COMPOSICION4;
        Carga_Composiciones_Idioma(c_prd.MODELO,'RU',v_comp_cn);
        -- Carga de la tabla REBAJAS

        commit;

        /*v_traza := 'Carga de la tabla REBAJAS';
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS,1);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_REBAJAS_2,2);
     Carga_Rebajas(c_prd.MODELO,c_prd.PRECIO_SALDO,3);*/

        v_existe := Busca_ExistPrd(c_prd.MODELO);
        --v_tendencia := Busca_Tendencia(c_prd.TENDENCIAS);

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          INSERT INTO PRODUCTOS
          (ID,
           REF_FABRICA,
           GENERICO,
           PVP,
           ESTILO,
           --FAMILIA,
           TENDENCIA,
           CODIGO_DISENYO,
           TIPO_ARTICULO,
           GENERO,
           BASE)
          VALUES
            (c_prd.MODELO,
             TRIM(c_prd.MODELO_FABRICA),
             NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
             translate(c_prd.PRECIO,',','.'),
             c_prd.LINEA,
             --c_prd.FAMILIA,
             c_prd.TENDENCIAS,
             c_prd.CODIGO_DISENYO,
             c_prd.TIPO_ARTICULO,
             c_prd.GENERO,
             c_prd.BASE);

          --insert into traza_jordi values('a001'||c_prd.MODELO,sysdate);

          COMMIT;

        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            UPDATE PRODUCTOS SET    REF_FABRICA = TRIM(c_prd.MODELO_FABRICA),
              GENERICO = NVL (replace(replace(RTRIM(c_prd.NOMBRE),chr(39),''),CHR(165),'Ñ'),c_prd.MODELO),
              PVP = translate(c_prd.PRECIO,',','.'),
              ESTILO = c_prd.LINEA,
              --FAMILIA = c_prd.FAMILIA,
              TENDENCIA = c_prd.TENDENCIAS,
              CODIGO_DISENYO = c_prd.CODIGO_DISENYO,
              TIPO_ARTICULO = c_prd.TIPO_ARTICULO,
              GENERO = c_prd.GENERO,
              BASE = c_prd.BASE
            WHERE ID = c_prd.MODELO;

            --insert into traza_jordi values('b001'||c_prd.MODELO,sysdate);

            COMMIT;

          END IF;

        END IF;

        v_existe := Busca_ExistPrdAlmacen(c_prd.MODELO, '075');--Forzamos alamacen China

        IF (v_existe = 0) THEN
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;
          ELSE
            v_fecha := SYSDATE;
          END IF;

          INSERT INTO PRODUCTO_ALMACEN
          (PRODUCTO,
           ALMACEN,
           FECHA_BAJA)
          VALUES
            (c_prd.MODELO,
             '075',
             v_fecha);

          --insert into traza_jordi values('a001'||c_prd.MODELO,sysdate);


          COMMIT;

        ELSE  --Entra aqui si el producto existe
          IF (c_prd.ACCION = 'A') THEN
            IF (c_prd.ACT_INACT = 'A') THEN
              v_fecha := '';
            ELSE
              v_fecha := SYSDATE;
            END IF;

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = v_fecha
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN='075';

            --insert into traza_jordi values('b001'||c_prd.MODELO,sysdate);

            COMMIT;

          ELSE

            UPDATE PRODUCTO_ALMACEN SET FECHA_BAJA = SYSDATE
            WHERE PRODUCTO = c_prd.MODELO AND ALMACEN='075';

            COMMIT;

          END IF;

        END IF;

        v_gtalla := Busca_GTalla(c_prd.MODELO);

        IF  (v_gtalla = 0) THEN
          -- insertar un nuevo producto en la tabla de grupos de tallas
          INSERT INTO REL_PRODUCTO_GTALLA
          (PRODUCTO, GRUPO_TALLA)
          VALUES
            (c_prd.MODELO, c_prd.GRUPO_TALLAS);
          COMMIT;
        ELSE
          -- Actualizar la tabla de greupos de tallas
          UPDATE REL_PRODUCTO_GTALLA
          SET GRUPO_TALLA=c_prd.GRUPO_TALLAS
          WHERE PRODUCTO=c_prd.MODELO;
          COMMIT;
        END IF;

      END LOOP;

      --ACT_TMP; -- ACTUALIZA BBDD TMP_ARTICULOS
      --ACT_TMP_PRECIOS;

      exception when others then

      DBMS_OUTPUT.PUT_LINE(SQLERRM);
      --insert into traza_jordi values('error en insercion de productos',sysdate);
    END Carga_Productos_XML;


  FUNCTION Busca_ExistNormaL (p_id IN VARCHAR2, p_idioma VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla NORMAS_LAVADO
    -- ----------------------------------------

    i_nl NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_nl FROM NORMAS_LAVADO WHERE ID = p_id AND IDIOMA = p_idioma;

      IF (i_nl = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistNormaL;


  PROCEDURE CARGA_NORMAS_LAVADO IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento que carga
    --                 la tabla NORMAS_LAVADO
    -- ----------------------------------------

    v_exist NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_NORMAS_LAVADO';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_NORMAS_LAVADO';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_NORMAS_LAVADO');

      FOR c_nl IN
      (SELECT * FROM MNGDES2.MNG_NORMAS_LAVADO)
      LOOP
        num_regs := num_regs + 1;

        v_exist := Busca_ExistNormaL(c_nl.COD_NORMA_LAVADO,'ES');

        IF (v_exist = 0) THEN
          IF (c_nl.NORMA_ES IS NOT NULL) THEN
            INSERT INTO NORMAS_LAVADO
            (ID,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_nl.COD_NORMA_LAVADO,
                   'ES',
                   c_nl.NORMA_ES);
          END IF;
        ELSE
          IF (c_nl.NORMA_ES IS NOT NULL) THEN
            UPDATE NORMAS_LAVADO SET DESCRIPCION=c_nl.NORMA_ES WHERE ID=c_nl.COD_NORMA_LAVADO AND IDIOMA ='ES';
          END IF;
        END IF;

        v_exist := Busca_ExistNormaL(c_nl.COD_NORMA_LAVADO,'IN');
        IF (v_exist = 0) THEN
          IF (c_nl.NORMA_IN IS NOT NULL) THEN
            INSERT INTO NORMAS_LAVADO
            (ID,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_nl.COD_NORMA_LAVADO,
                   'IN',
                   c_nl.NORMA_IN);
          END IF;
        ELSE
          IF (c_nl.NORMA_IN IS NOT NULL) THEN
            UPDATE NORMAS_LAVADO SET DESCRIPCION=c_nl.NORMA_IN WHERE ID=c_nl.COD_NORMA_LAVADO AND IDIOMA ='IN';
          END IF;
        END IF;

        v_exist := Busca_ExistNormaL(c_nl.COD_NORMA_LAVADO,'FR');
        IF (v_exist = 0) THEN
          IF (c_nl.NORMA_FR IS NOT NULL) THEN
            INSERT INTO NORMAS_LAVADO
            (ID,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_nl.COD_NORMA_LAVADO,
                   'FR',
                   c_nl.NORMA_FR);
          END IF;
        ELSE
          IF (c_nl.NORMA_FR IS NOT NULL) THEN
            UPDATE NORMAS_LAVADO SET DESCRIPCION=c_nl.NORMA_FR WHERE ID=c_nl.COD_NORMA_LAVADO AND IDIOMA ='FR';
          END IF;
        END IF;

        v_exist := Busca_ExistNormaL(c_nl.COD_NORMA_LAVADO,'AL');
        IF (v_exist = 0) THEN
          IF (c_nl.NORMA_AL IS NOT NULL) THEN
            INSERT INTO NORMAS_LAVADO
            (ID,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_nl.COD_NORMA_LAVADO,
                   'AL',
                   c_nl.NORMA_AL);
          END IF;
        ELSE
          IF (c_nl.NORMA_AL IS NOT NULL) THEN
            UPDATE NORMAS_LAVADO SET DESCRIPCION=c_nl.NORMA_AL WHERE ID=c_nl.COD_NORMA_LAVADO AND IDIOMA ='AL';
          END IF;
        END IF;

        v_exist := Busca_ExistNormaL(c_nl.COD_NORMA_LAVADO,'CA');
        IF (v_exist = 0) THEN
          IF (c_nl.NORMA_CA IS NOT NULL) THEN
            INSERT INTO NORMAS_LAVADO
            (ID,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_nl.COD_NORMA_LAVADO,
                   'CA',
                   c_nl.NORMA_CA);
          END IF;
        ELSE
          IF (c_nl.NORMA_CA IS NOT NULL) THEN
            UPDATE NORMAS_LAVADO SET DESCRIPCION=c_nl.NORMA_CA WHERE ID=c_nl.COD_NORMA_LAVADO AND IDIOMA ='CA';
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_NORMAS_LAVADO;

  FUNCTION Busca_ExistPrdSimb (p_prd IN VARCHAR2,p_simb IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla PRODUCTO_SIMBOLO
    -- ----------------------------------------

    i_ps NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_ps FROM PRODUCTO_SIMBOLO WHERE PRODUCTO = p_prd AND SIMBOLO = p_simb;

      IF (i_ps = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;
    END Busca_ExistPrdSimb;

  PROCEDURE CARGA_PRODUCTO_SIMBOLO IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento que carga
    --                 la tabla PRODUCTO_SIMBOLO
    -- ----------------------------------------

    v_exist NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PRODUCTO_SIMBOLO';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PRODUCTO_SIMBOLO';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PRODUCTOS_SIMBOLOS_LAVADO');

      FOR c_ps IN
      (SELECT MODELO,SIMBOLO_L1,SIMBOLO_L2,SIMBOLO_L3,SIMBOLO_L4,SIMBOLO_L5
       FROM MNGDES2.MNG_PRODUCTOS_SIMBOLOS_LAVADO)
      LOOP
        num_regs := num_regs + 1;

        v_exist := Busca_ExistPrdSimb(c_ps.MODELO,c_ps.SIMBOLO_L1);
        IF (v_exist = 0) THEN
          IF (c_ps.SIMBOLO_L1 IS NOT NULL) THEN
            INSERT INTO PRODUCTO_SIMBOLO
            (PRODUCTO,
             SIMBOLO)
            VALUES(c_ps.MODELO,
                   c_ps.SIMBOLO_L1);
          END IF;
        END IF;

        v_exist := Busca_ExistPrdSimb(c_ps.MODELO,c_ps.SIMBOLO_L2);
        IF (v_exist = 0) THEN
          IF (c_ps.SIMBOLO_L2 IS NOT NULL) THEN
            INSERT INTO PRODUCTO_SIMBOLO
            (PRODUCTO,
             SIMBOLO)
            VALUES(c_ps.MODELO,
                   c_ps.SIMBOLO_L2);
          END IF;
        END IF;

        v_exist := Busca_ExistPrdSimb(c_ps.MODELO,c_ps.SIMBOLO_L3);
        IF (v_exist = 0) THEN
          IF (c_ps.SIMBOLO_L3 IS NOT NULL) THEN
            INSERT INTO PRODUCTO_SIMBOLO
            (PRODUCTO,
             SIMBOLO)
            VALUES(c_ps.MODELO,
                   c_ps.SIMBOLO_L3);
          END IF;
        END IF;

        v_exist := Busca_ExistPrdSimb(c_ps.MODELO,c_ps.SIMBOLO_L4);
        IF (v_exist = 0) THEN
          IF (c_ps.SIMBOLO_L4 IS NOT NULL) THEN
            INSERT INTO PRODUCTO_SIMBOLO
            (PRODUCTO,
             SIMBOLO)
            VALUES(c_ps.MODELO,
                   c_ps.SIMBOLO_L4);
          END IF;
        END IF;

        v_exist := Busca_ExistPrdSimb(c_ps.MODELO,c_ps.SIMBOLO_L5);
        IF (v_exist = 0) THEN
          IF (c_ps.SIMBOLO_L5 IS NOT NULL) THEN
            INSERT INTO PRODUCTO_SIMBOLO
            (PRODUCTO,
             SIMBOLO)
            VALUES(c_ps.MODELO,
                   c_ps.SIMBOLO_L5);
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_PRODUCTO_SIMBOLO;

  FUNCTION Busca_ExistPrdNL (p_prd IN VARCHAR2,p_nl IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla PRODUCTO_NORMA_LAVADO
    -- ----------------------------------------

    i_pnl NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_pnl FROM PRODUCTO_NORMA_LAVADO WHERE PRODUCTO = p_prd AND NORMA_LAVADO = p_nl;

      IF (i_pnl = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;
    END Busca_ExistPrdNL;


  PROCEDURE CARGA_PRODUCTO_NORMA_LAVADO IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento que carga
    --                 la tabla PRODUCTO_NORMA_LAVADO
    -- ----------------------------------------

    v_exist NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PRODUCTO_NORMA_LAVADO';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PRODUCTO_NORMA_LAVADO';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PRODUCTOS_NORMAS_LAVADO');

      FOR c_pnl IN
      (SELECT MODELO,COD_NORMA
       FROM MNGDES2.MNG_PRODUCTOS_NORMAS_LAVADO)
      LOOP
        num_regs := num_regs + 1;

        v_exist := Busca_ExistPrdNL(c_pnl.MODELO, c_pnl.COD_NORMA);

        IF (v_exist = 0) THEN
          IF (c_pnl.MODELO IS NOT NULL) THEN
            IF (c_pnl.COD_NORMA IS NOT NULL) THEN
              INSERT INTO PRODUCTO_NORMA_LAVADO
              (PRODUCTO,
               NORMA_LAVADO)
              VALUES(c_pnl.MODELO,
                     c_pnl.COD_NORMA);
            END IF;
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_PRODUCTO_NORMA_LAVADO;

  FUNCTION Busca_ExistDscr (p_prd IN VARCHAR2,p_idioma IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla DESCRIPCIONES
    -- ----------------------------------------

    i_dscr NUMBER := 0;

    BEGIN
      SELECT COUNT(PRODUCTO) INTO i_dscr FROM DESCRIPCIONES WHERE PRODUCTO = p_prd AND IDIOMA = p_idioma;

      IF (i_dscr = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistDscr;

  PROCEDURE CARGA_DESCRIPCIONES IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento que carga
    --                 la tabla DESCRIPCIONES
    -- ----------------------------------------

    v_exist NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_DESCRIPCIONES';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_DESCRIPCIONES';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_VARIOS');

      FOR c_dscr IN
      (SELECT MODELO,DESC_ART_ES,DESC_ART_IN,DESC_ART_FR,DESC_ART_AL, DESC_ART_CA
       FROM MNGDES2.MNG_VARIOS)
      LOOP
        num_regs := num_regs + 1;

        v_exist := Busca_ExistDscr(c_dscr.MODELO,'ES');
        IF (v_exist = 0) THEN
          IF (c_dscr.DESC_ART_ES IS NOT NULL) THEN
            INSERT INTO DESCRIPCIONES
            (PRODUCTO,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_dscr.MODELO,
                   'ES',
                   c_dscr.DESC_ART_ES);
          END IF;
        ELSE
          IF (c_dscr.DESC_ART_ES IS NOT NULL) THEN
            UPDATE DESCRIPCIONES SET DESCRIPCION = c_dscr.DESC_ART_ES WHERE PRODUCTO = c_dscr.MODELO AND IDIOMA ='ES';
          END IF;
        END IF;

        v_exist := Busca_ExistDscr(c_dscr.MODELO,'IN');
        IF (v_exist = 0) THEN
          IF (c_dscr.DESC_ART_IN IS NOT NULL) THEN
            INSERT INTO DESCRIPCIONES
            (PRODUCTO,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_dscr.MODELO,
                   'IN',
                   c_dscr.DESC_ART_IN);
          END IF;
        ELSE
          IF (c_dscr.DESC_ART_IN IS NOT NULL) THEN
            UPDATE DESCRIPCIONES SET DESCRIPCION = c_dscr.DESC_ART_IN WHERE PRODUCTO = c_dscr.MODELO AND IDIOMA ='IN';
          END IF;
        END IF;

        v_exist := Busca_ExistDscr(c_dscr.MODELO,'FR');
        IF (v_exist = 0) THEN
          IF (c_dscr.DESC_ART_FR IS NOT NULL) THEN
            INSERT INTO DESCRIPCIONES
            (PRODUCTO,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_dscr.MODELO,
                   'FR',
                   c_dscr.DESC_ART_FR);
          END IF;
        ELSE
          IF (c_dscr.DESC_ART_FR IS NOT NULL) THEN
            UPDATE DESCRIPCIONES SET DESCRIPCION = c_dscr.DESC_ART_FR WHERE PRODUCTO = c_dscr.MODELO AND IDIOMA ='FR';
          END IF;
        END IF;

        v_exist := Busca_ExistDscr(c_dscr.MODELO,'AL');
        IF (v_exist = 0) THEN
          IF (c_dscr.DESC_ART_AL IS NOT NULL) THEN
            INSERT INTO DESCRIPCIONES
            (PRODUCTO,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_dscr.MODELO,
                   'AL',
                   c_dscr.DESC_ART_AL);
          END IF;
        ELSE
          IF (c_dscr.DESC_ART_AL IS NOT NULL) THEN
            UPDATE DESCRIPCIONES SET DESCRIPCION = c_dscr.DESC_ART_AL WHERE PRODUCTO = c_dscr.MODELO AND IDIOMA ='AL';
          END IF;
        END IF;

        v_exist := Busca_ExistDscr(c_dscr.MODELO,'CA');
        IF (v_exist = 0) THEN
          IF (c_dscr.DESC_ART_CA IS NOT NULL) THEN
            INSERT INTO DESCRIPCIONES
            (PRODUCTO,
             IDIOMA,
             DESCRIPCION)
            VALUES(c_dscr.MODELO,
                   'CA',
                   c_dscr.DESC_ART_CA);
          END IF;
        ELSE
          IF (c_dscr.DESC_ART_CA IS NOT NULL) THEN
            UPDATE DESCRIPCIONES SET DESCRIPCION = c_dscr.DESC_ART_CA WHERE PRODUCTO = c_dscr.MODELO AND IDIOMA ='CA';
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_DESCRIPCIONES;

  FUNCTION Busca_ExistArticulo (p_id IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla ARTICULOS
    -- ----------------------------------------

    i_art NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_art FROM ARTICULOS WHERE ID = p_id;

      IF (i_art = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistArticulo;

  FUNCTION Busca_ExistArticuloAlmacen (p_id IN VARCHAR2, p_almacen IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla ARTICULOS ALMACEN
    -- ----------------------------------------

    i_art NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_art FROM ARTICULOS_ALMACEN WHERE ID = p_id and ALMACEN=p_almacen;

      IF (i_art = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistArticuloAlmacen;


  FUNCTION Busca_Existe_FechaAltaArticulo (p_id IN VARCHAR2, p_almacen IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion que nos dice si el articulo tiene fecha de alta.
    --                 Es decir, si el producto ya habia recibido stock por primera vez.
    -- ----------------------------------------

    i_art NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_art
      FROM ARTICULOS_ALMACEN AA
        LEFT JOIN FECHA_ALTA_ARTICULOS_ALMACEN FA ON FA.ARTICULO = AA.ID AND FA.ALMACEN = AA.ALMACEN
      WHERE AA.ID = p_id and AA.ALMACEN= p_almacen
            AND AA.STOCK_REAL > 0 AND FECHA_BAJA IS NULL AND FECHA_ALTA IS NULL;

      IF (i_art = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_Existe_FechaAltaArticulo;

  PROCEDURE Carga_Articulos IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de carga
    --              de datos de Articulos.
    -- ----------------------------------------
    v_existe NUMBER;
    v_gtalla VARCHAR2(2):='er';--error: si no existe ningún registro en REL_PRODUCTO_GTALLA guardará este valor que significará error en la carga(Emilio 14/09/2006)
    v_fecha DATE;
    v_traza VARCHAR2(2000) := '';
    v_existe_fecha_alta NUMBER; -- Nos dira si ese articulo ya esta insertado en la tabla fecha_alta_articulos_almacen.

    v_almacen_regulariza char(3);
    pos_codi number;
    txt_id   varchar2(128);

    txt_almacen    char(3);
    txt_temporada  char(1);

    num_alma   number;
    txt_alma   varchar2(256);

    CURSOR c_art IS
      SELECT *
      FROM MNGDES2.MNG_ARTICULOS ma
      ORDER BY ma.fecha ASC;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_ARTICULOS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_ARTICULOS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;

    num_inserts             number;
    num_updates             number;
    num_inserts2            number;
    num_updates2            number;
    num_inserts3            number;
    id_exec                 number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      num_inserts := 0;
      num_updates := 0;
      num_inserts2 := 0;
      num_updates2 := 0;
      num_inserts3 := 0;

      pos_codi := 0;

      -- *KIKU* 27/01/2015 Genero Dinàmicament SUB_ID_PROC
      BEGIN
        PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Genero SUB_ID_PROC');

        SELECT almacen, substr(modelo,1,1) as temporada INTO txt_almacen, txt_temporada
        FROM MNGDES2.MNG_ARTICULOS
        where rownum = 1;

        EXCEPTION
        when no_data_found then
        txt_almacen := '???';
        txt_temporada := '?';
      END;

      -- *KIKU* 8/04/2015 Crida a INICI_EXECUCIO
      PCKG_EXPLOTACIO.INICI_EXECUCIO(id_proc,sub_id_proc|| '_' || txt_almacen || '_' || txt_temporada,'',id_exec,'ORACLE');

      -- *KIKU* 26/02/2015 Cerco els magatzems al principi pq si dóna error també donar aquesta info
      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cerco Magatzems');
      -- *KIKU* 16/02/2015 Sembla que hi ha més d'un magatzem en un sol fitxer... genero log per validar que sigui així
      num_alma := 0;
      txt_alma := '';
      begin
        FOR c_alma_temp IN (SELECT distinct almacen, substr(modelo,1,1) as temporada FROM MNGDES2.MNG_ARTICULOS)
        LOOP
          num_alma := num_alma + 1;

          if num_alma = 1 then
            txt_alma := '(';
          else
            txt_alma := txt_alma || ', ';
          end if;

          txt_alma := txt_alma || c_alma_temp.almacen || '-' || c_alma_temp.temporada;
        END LOOP;

        if num_alma > 1 then
          txt_alma := txt_alma || ')';
        end if;

        exception
        when no_data_found then
        null;
      end;

      -- *KIKU* 23/3/2015 Guardo informació resum del contingut que carregaré
      -- *KIKU*  8/4/2015 Incorporo ID_EXEC a la taula per lligar amb EXP_LOG_EXECUCIONS
      insert into exp_control_carga_articulos
        select sysdate as hora_carga, almacen, temporada, articulos, 0, id_exec from
          (
            SELECT almacen, substr(modelo,1,1) as temporada, count(*) as articulos FROM MNGDES2.MNG_ARTICULOS
            group by almacen, substr(modelo,1,1)
          );


      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_ARTICULOS');

      FOR v_c_art IN c_art LOOP
        FOR c_gtalla IN
        (SELECT GRUPO_TALLA
         FROM REL_PRODUCTO_GTALLA mp
         WHERE mp.PRODUCTO = v_c_art.MODELO)
        LOOP
          v_gtalla := c_gtalla.GRUPO_TALLA;
        END LOOP;

        num_regs := num_regs + 1;

        pos_codi := 1;

        v_existe := Busca_ExistArticulo(v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR);

        pos_codi := 2;

        IF (v_existe = 0) THEN --no existe en articulos por que se hara un INSERT
          IF (v_c_art.ACCION = 'A') THEN
            v_fecha := '';
          ELSIF (v_c_art.ACCION = 'B') THEN
            v_fecha := SYSDATE;
          ELSIF (v_c_art.ACCION = 'S') THEN
            v_fecha := '';
          END IF;

          IF (v_c_art.ACCION IN ('A','B','S')) THEN
            num_inserts := num_inserts + 1;

            pos_codi := 3;

            INSERT INTO ARTICULOS
            (ID,
             PRODUCTO,
             TALLA,
             GRUPO_TALLA,
             COLOR,
             PESO_BRUTO,
             BLOQUE,
             EAN13,
             CODIGO_NRF)
            VALUES
              (v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR,
               v_c_art.MODELO,
               v_c_art.TALLA,
               v_gtalla,
               v_c_art.COLOR,
               v_c_art.PESO_BRUTO,
               v_c_art.BLOQUE,
               v_c_art.EAN13,
               v_c_art.codigo_nrf);
          END IF;

        ELSE /* JJCF - Si existe el artículo hay que actualizar sus campos dependiendo del campo ACCION */
          --RedMine #19104 hacemos que siempre se updatee el grupo_talla para evitar incongruencia con la tabla rel_producto_gtalla
          --IF ((v_c_art.ACCION = 'A') OR (v_c_art.ACCION = 'S')) THEN
          num_updates := num_updates + 1;

          pos_codi := 4;

          UPDATE ARTICULOS SET GRUPO_TALLA = v_gtalla,
            BLOQUE=v_c_art.BLOQUE,
            PESO_BRUTO = v_c_art.PESO_BRUTO, EAN13=v_c_art.EAN13, CODIGO_NRF=v_c_art.CODIGO_NRF
          WHERE ID = v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR;
          --END IF;
        END IF;

        pos_codi := 5;

        v_existe := Busca_ExistArticuloAlmacen(v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR, v_c_art.ALMACEN );

        IF (v_existe = 0) THEN --no existe en articulos por que se hara un INSERT
          IF (v_c_art.ACCION = 'A') THEN
            v_fecha := '';
          ELSIF (v_c_art.ACCION = 'B') THEN
            v_fecha := SYSDATE;
          ELSIF (v_c_art.ACCION = 'S') THEN
            v_fecha := '';
          END IF;

          IF (v_c_art.ACCION IN ('A','B','S')) THEN
            num_inserts2 := num_inserts2 + 1;

            pos_codi := 6;

            -- *KIKU* 25/3/2015 Guardo informació de quant s'Insereix l'article a la columna FECHA_ACT
            INSERT INTO ARTICULOS_ALMACEN
            (ID,
             ALMACEN,
             FECHA_BAJA,
             STOCK_REAL,
             STOCK_MINIMO,
             STOCK_IDEAL,
             FECHA_ACT)
            VALUES
              (v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR,
               v_c_art.ALMACEN,
               v_fecha,
               v_c_art.STK_REAL,
               v_c_art.STK_MINIMO,
               v_c_art.STK_IDEAL,
               SYSDATE);
          END IF;
        ELSE /* JJCF - Si existe el artículo hay que actualizar sus campos dependiendo del campo ACCION */
          IF (v_c_art.ACCION = 'A') THEN
            v_fecha := '';
          ELSIF (v_c_art.ACCION = 'B') THEN
            -- KIKU 16/6/2014 Si és una baixa i fecha_baja ja està informada, deixar la data que ja hi havia. Si no hi havia cap, força la data sistema
            -- v_fecha := SYSDATE;
            SELECT nvl(fecha_baja,sysdate) INTO v_fecha FROM ARTICULOS_ALMACEN WHERE ID = v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR and ALMACEN= v_c_art.ALMACEN;
          ELSIF (v_c_art.ACCION = 'S') THEN
            v_fecha := '';
          END IF;

          num_updates2 := num_updates2 + 1;

          pos_codi := 7;
          txt_id := to_char(v_c_art.MODELO)||','||to_char(v_c_art.TALLA)||','||to_char(v_c_art.COLOR)||','||to_char(v_c_art.ALMACEN);

          -- *KIKU* 25/3/2015 Guardo informació de quant s'Updateja l'article a la columna FECHA_ACT
          UPDATE ARTICULOS_ALMACEN SET
            STOCK_REAL = v_c_art.STK_REAL,
            STOCK_IDEAL = v_c_art.STK_IDEAL,
            STOCK_MINIMO = v_c_art.STK_MINIMO,
            FECHA_BAJA=v_fecha,
            FECHA_ACT=SYSDATE
          WHERE ID = v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR AND
                ALMACEN=v_c_art.ALMACEN;
        END IF;

        --Miramos si es la primera vez que recibimos stock de este articulos y lo insertamos en la tabla fecha_alta_articulos_almacen.
        v_existe_fecha_alta := Busca_Existe_FechaAltaArticulo(v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR, v_c_art.ALMACEN );
        IF (v_existe_fecha_alta = 1) THEN
          num_inserts3 := num_inserts3 + 1;

          pos_codi := 8;

          INSERT INTO FECHA_ALTA_ARTICULOS_ALMACEN
          (ARTICULO,
           ALMACEN,
           FECHA_ALTA)
          VALUES
            (v_c_art.MODELO||v_c_art.TALLA||v_c_art.COLOR,
             v_c_art.ALMACEN,
             SYSDATE);
        END IF;

        -- *KIKU* 26/02/2015 Faig commit fila a fila per evitar el Deadlock
        COMMIT;
      END LOOP;

      pos_codi := 9;

      -- KIKU 3/12/2013 Sols farà un commit al final
      -- *KIKU* 26/02/2013 Faig que faci el commit cada fila per evitar els Deadlock amb altres processos que actualitzan la taula ARTICULOS_ALMACEN
      --                   Es veu per la traça Oracle que una sentència d'un tomcat ens provoca els Deadlock
      --
      --                           UPDATE ARTICULOS_ALMACEN SET STOCK_REAL = STOCK_REAL - 1 WHERE ID = :B2 AND ALMACEN = :B1
      --
      --COMMIT;

      pos_codi := 10;

      -- *KIKU* 12/12/2014 En determinats moments no hi ha informació a la taula MNGDES2.MNG_ARTICULOS
      begin
        /* KIKU 3/12/2013 Regularització stock per falta picking */
        --select almacen into v_almacen_regulariza from MNGDES2.MNG_ARTICULOS where rownum = 1;

        -- *KIKU* 23/02/2015 Com que pot haver-hi més d'un magatzem dins de la taula MNG_ARTICULOS, crido a REGULARITZA_STOCK tantes vegades com magatzems diferents
        FOR c_alma_temp IN (SELECT distinct almacen FROM MNGDES2.MNG_ARTICULOS)
        LOOP
          REGULARIZA_STOCK(c_alma_temp.almacen);
        END LOOP;

        exception
        when no_data_found then
        null;
      end;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';
      status_exe_proc := status_exe_proc || ' (I:' ||to_char(num_inserts)||' U:'||to_char(num_updates)||
                         ' I2:' ||to_char(num_inserts2)||' U2:'||to_char(num_updates2)||
                         ' I3:' ||to_char(num_inserts3)||')';

      if num_alma > 1 then
        status_exe_proc := status_exe_proc || ' *** MAGATZEM/TEMPORADA DIFERENTS!! ' || txt_alma || ' ***';
      end if;

      ltimestamp_end := systimestamp;
      --PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc|| '_' || txt_almacen || '_' || txt_temporada,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);
      PCKG_EXPLOTACIO.FI_EXECUCIO (id_proc,sub_id_proc|| '_' || txt_almacen || '_' || txt_temporada,id_exec,'O',status_exe_proc);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      --ACT_TMP; -- ACTUALIZA BBDD TEMPORALES
      --ACT_TMP_PRECIOS; --ACTUALIZAS BBDD PRECIOS TEMPORALES

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL (Pos:'||trim(to_char(pos_codi))||
                         ' TXT_ID:'||txt_id||
                         '): '||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      -- *KIKU* 26/2/2015 Trec informació dels magatzems si hi ha més d'un
      if num_alma > 1 then
        status_exe_proc := status_exe_proc || ' *** MAGATZEM/TEMPORADA DIFERENTS!! ' || txt_alma || ' ***';
      end if;

      --PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc|| '_' || txt_almacen || '_' || txt_temporada,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);
      PCKG_EXPLOTACIO.FI_EXECUCIO (id_proc,sub_id_proc|| '_' || txt_almacen || '_' || txt_temporada,id_exec,'E',status_exe_proc);

      -- *KIKU* 3/2/2015 Per si de cas i es queda alguna cosa enganxada :(
      ROLLBACK;

    END Carga_Articulos;


  FUNCTION Busca_ExistColor (p_gcolor IN VARCHAR2,p_color IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla COLORES
    -- ----------------------------------------

    i_color NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_color FROM COLORES WHERE GRUPO_COLOR = p_gcolor AND ID = p_color AND IDIOMA = 'ES';

      IF (i_color = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;
    END Busca_ExistColor;

  PROCEDURE CARGA_COLORES IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de colores.
    -- ----------------------------------------
    v_existe NUMBER;

    CURSOR c_color IS
      SELECT *
      FROM MNGDES2.MNG_COLORES_CATALOGO mcc
      ORDER BY mcc.COLOR_BASICO;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_COLORES';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_COLORES';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_COLORES_CATALOGO');

      FOR v_c_color IN c_color
      LOOP
        num_regs := num_regs + 1;

        FOR c_gcolor IN
        (SELECT DISTINCT COLOR_BASICO,COD_GRUPOCOLOR
         FROM MNGDES2.MNG_COLORES mc
         WHERE mc.COLOR_BASICO = v_c_color.COLOR_BASICO)
        LOOP
          v_existe := Busca_ExistColor(c_gcolor.COD_GRUPOCOLOR,v_c_color.COLOR_BASICO);

          IF (v_existe = 0 and c_gcolor.COD_GRUPOCOLOR Is Not Null) THEN
            INSERT INTO COLORES
            (ID,
             IDIOMA,
             GRUPO_COLOR,
             DESCRIPCION)
            VALUES    (v_c_color.COLOR_BASICO,
                       'ES',
                       c_gcolor.COD_GRUPOCOLOR,
                       v_c_color.DESCRIPCION_ES);

            INSERT INTO COLORES
            (ID,
             IDIOMA,
             GRUPO_COLOR,
             DESCRIPCION)
            VALUES    (v_c_color.COLOR_BASICO,
                       'IN',
                       c_gcolor.COD_GRUPOCOLOR,
                       v_c_color.DESCRIPCION_IN);

            INSERT INTO COLORES
            (ID,
             IDIOMA,
             GRUPO_COLOR,
             DESCRIPCION)
            VALUES    (v_c_color.COLOR_BASICO,
                       'FR',
                       c_gcolor.COD_GRUPOCOLOR,
                       v_c_color.DESCRIPCION_FR);

            INSERT INTO COLORES
            (ID,
             IDIOMA,
             GRUPO_COLOR,
             DESCRIPCION)
            VALUES    (v_c_color.COLOR_BASICO,
                       'AL',
                       c_gcolor.COD_GRUPOCOLOR,
                       v_c_color.DESCRIPCION_AL);

            INSERT INTO COLORES
            (ID,
             IDIOMA,
             GRUPO_COLOR,
             DESCRIPCION)
            VALUES    (v_c_color.COLOR_BASICO,
                       'CA',
                       c_gcolor.COD_GRUPOCOLOR,
                       v_c_color.DESCRIPCION_CA);
          END IF;
        END LOOP;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END Carga_Colores;


  FUNCTION Busca_ExistRelColorGcolor (p_gcolor IN VARCHAR2, p_color IN VARCHAR2, p_generico IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla REL_COLOR_GCOLORES
    -- ----------------------------------------

    i_gcolor NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_gcolor FROM REL_COLOR_GCOLOR WHERE GRUPO_COLOR = p_gcolor AND COLOR = p_color AND GENERICO = p_generico;

      IF (i_gcolor = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistRelColorGcolor;


  PROCEDURE CARGA_REL_COLOR_GCOLOR IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de relacion color grupo color.
    -- ----------------------------------------
    v_existe NUMBER;

    CURSOR c_color IS
      SELECT * FROM MNGDES2.MNG_COLORES;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_REL_COLOR_GCOLOR';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_REL_COLOR_GCOLOR';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;

    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------


    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_COLORES');

      FOR v_c_color IN c_color
      LOOP
        num_regs := num_regs + 1;

        v_existe := Busca_ExistRelColorGColor(v_c_color.COD_GRUPOCOLOR,v_c_color.COLOR_BASICO, v_c_color.GENERICO);

        IF (v_existe = 0) THEN
          INSERT INTO REL_COLOR_GCOLOR
          (COLOR,
           GRUPO_COLOR,
           GENERICO)
          VALUES    (LPAD(v_c_color.COLOR_BASICO,2,0),
                     v_c_color.COD_GRUPOCOLOR,
                     v_c_color.GENERICO);
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_REL_COLOR_GCOLOR;

  FUNCTION Busca_ExistGcolor (p_gcolor IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla GRUPO_COLORES
    -- ----------------------------------------

    i_gcolor NUMBER := 0;

    BEGIN
      SELECT COUNT(ID) INTO i_gcolor FROM GRUPO_COLORES WHERE ID = p_gcolor AND IDIOMA = 'ES';

      IF (i_gcolor = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistGcolor;

  PROCEDURE CARGA_GRUPOCOLORES IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de grupo colores.
    -- ----------------------------------------
    v_existe NUMBER;

    CURSOR c_gcolor IS
      SELECT *
      FROM MNGDES2.MNG_GRUPO_COLORES;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_GRUPOCOLORES';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_GRUPOCOLORES';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_GRUPO_COLORES');

      FOR v_c_gcolor IN c_gcolor
      LOOP
        num_regs := num_regs + 1;

        v_existe := Busca_ExistGcolor(v_c_gcolor.COD_GRUPOCOLOR);

        IF (v_existe = 0) THEN
          INSERT INTO GRUPO_COLORES
          (ID,
           IDIOMA,
           DESCRIPCION)
          VALUES    (v_c_gcolor.COD_GRUPOCOLOR,
                     'ES',
                     REPLACE(LTRIM(RTRIM(v_c_gcolor.DESCRIPCION_ES)),' ','&nbsp;'));

          INSERT INTO GRUPO_COLORES
          (ID,
           IDIOMA,
           DESCRIPCION)
          VALUES    (v_c_gcolor.COD_GRUPOCOLOR,
                     'IN',
                     REPLACE(LTRIM(RTRIM(v_c_gcolor.DESCRIPCION_IN)),' ','&nbsp;'));

          INSERT INTO GRUPO_COLORES
          (ID,
           IDIOMA,
           DESCRIPCION)
          VALUES    (v_c_gcolor.COD_GRUPOCOLOR,
                     'FR',
                     REPLACE(LTRIM(RTRIM(v_c_gcolor.DESCRIPCION_FR)),' ','&nbsp;'));

          INSERT INTO GRUPO_COLORES
          (ID,
           IDIOMA,
           DESCRIPCION)
          VALUES    (v_c_gcolor.COD_GRUPOCOLOR,
                     'AL',
                     REPLACE(LTRIM(RTRIM(v_c_gcolor.DESCRIPCION_AL)),' ','&nbsp;'));

          INSERT INTO GRUPO_COLORES
          (ID,
           IDIOMA,
           DESCRIPCION)
          VALUES    (v_c_gcolor.COD_GRUPOCOLOR,
                     'CA',
                     REPLACE(LTRIM(RTRIM(v_c_gcolor.DESCRIPCION_CA)),' ','&nbsp;'));
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_GRUPOCOLORES;


  FUNCTION Busca_EquivTalla (p_talla IN NUMBER) RETURN VARCHAR2 IS
    -- ----------------------------------------
    -- Descripcion: Funcion de busqueda de la
    --                 descripcion de la talla.
    -- ----------------------------------------

    v_desc VARCHAR2(3);

    BEGIN
      SELECT TALLA INTO v_desc FROM MNGDES2.MNG_EQUIV_TALLAS WHERE COD_TALLA = p_talla;

      RETURN v_desc;

      EXCEPTION
      WHEN OTHERS THEN
      v_desc := '';
      RETURN v_desc;
    END Busca_EquivTalla;


  FUNCTION Busca_ExistTalla (p_gtalla IN NUMBER,p_talla IN NUMBER) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla TALLAS
    -- ----------------------------------------

    i_grup NUMBER := 0;

    BEGIN
      SELECT COUNT(GRUPO) INTO i_grup FROM TALLAS WHERE GRUPO = p_gtalla AND TALLA = p_talla;

      IF (i_grup = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistTalla;

  PROCEDURE CARGA_TALLAS IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de TALLAS.
    -- ----------------------------------------
    v_desc VARCHAR2(3);
    v_existe NUMBER;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_TALLAS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_TALLAS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_GTALLAS');

      FOR c_talla IN
      (SELECT *
       FROM MNGDES2.MNG_GTALLAS gt
       ORDER BY gt.COD_GRUPO)
      LOOP
        num_regs := num_regs + 1;

        IF (c_talla.TALLA_1 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_1);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_1);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_1,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_2 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_2);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_2);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_2,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_3 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_3);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_3);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_3,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_4 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_4);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_4);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_4,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_5 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_5);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_5);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_5,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_6 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_6);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_6);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_6,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_7 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_7);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_7);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_7,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_8 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_8);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_8);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_8,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_9 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_9);
          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_9);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_9,
                       v_desc);
          END IF;
        END IF;

        IF (c_talla.TALLA_10 <> '  ') THEN
          v_existe  := Busca_ExistTalla(c_talla.COD_GRUPO,c_talla.TALLA_10);

          IF (v_existe = 0) THEN
            v_desc := Busca_EquivTalla(c_talla.TALLA_10);
            INSERT INTO TALLAS
            (GRUPO,
             TALLA,
             DESCRIPCION)
            VALUES    (c_talla.COD_GRUPO,
                       c_talla.TALLA_10,
                       v_desc);
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_TALLAS;

  PROCEDURE CARGA_CAMBIOS IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de cambios.
    -- ----------------------------------------
    v_id NUMBER := 0;

    CURSOR c_cambio IS
      SELECT *
      FROM MNGDES2.MNG_CAMBIOS
      ORDER BY PEDIDO;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_CAMBIOS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_CAMBIOS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);

    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MAX CAMBIOS');

      BEGIN
        SELECT MAX(ID)
        INTO v_id
        FROM CAMBIOS;
        EXCEPTION
        WHEN NO_DATA_FOUND THEN
        v_id := 0;
      END;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_CAMBIOS');

      FOR v_c_cambio IN c_cambio LOOP
        num_regs := num_regs + 1;

        v_id := v_id +1;

        INSERT INTO CAMBIOS
        (ID,
         SUBPEDIDO,
         PEDIDO,
         COD_TIENDA,
         PRODUCTO,
         TALLA,
         COLOR,
         CANTIDAD,
         DEVOLUCION,
         DEV_TRANSPORTE,
         ESTADO,
         MOTIVO,
         PRODUCTO_NUEVO,
         TALLA_NUEVO,
         COLOR_NUEVO,
         ALBARAN,
         FECHA,
         ALMACEN)
        VALUES    (v_id,
          v_c_cambio.SUBPEDIDO,
          v_c_cambio.PEDIDO,
          v_c_cambio.COD_TIENDA,
          v_c_cambio.MODELO,
          v_c_cambio.TALLA,
          v_c_cambio.COLOR,
          v_c_cambio.CANTIDAD,
          v_c_cambio.IMPORTE_DEVUELTO,
          v_c_cambio.IMPORTE_DEV_TRANSPORTE,
          v_c_cambio.ACCION,
                   v_c_cambio.MOTIVO,
                   v_c_cambio.MODELO_NUEVO,
                   v_c_cambio.TALLA_NUEVO,
                   v_c_cambio.COLOR_NUEVO,
                   v_c_cambio.NUMERO_ALBARAN,
                   TO_DATE(v_c_cambio.FECHA_CAMBIO,'yyyymmdd'),
                   v_c_cambio.ALMACEN);
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

      DBMS_OUTPUT.PUT_LINE(SQLERRM);
    END Carga_Cambios;

  FUNCTION Busca_ExistPrecio_Articulos (p_producto IN VARCHAR2, p_pais IN VARCHAR2, p_tarifa IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el registro en la tabla PRECIOS
    -- ----------------------------------------

    i_producto NUMBER := 0;

    BEGIN
      SELECT COUNT(PRE.PRODUCTO)
      INTO i_producto
      FROM PRECIOS PRE
      WHERE PRE.PAIS=p_pais
            AND PRE.TARIFA=p_tarifa
            AND PRE.PRODUCTO=p_producto;

      IF (i_producto = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrecio_Articulos;

  FUNCTION Busca_ExistPrecio_Articulos_o (p_producto IN VARCHAR2, p_pais IN VARCHAR2, p_tarifa IN VARCHAR2) RETURN NUMBER IS

    i_producto NUMBER := 0;

    BEGIN
      SELECT COUNT(PRE.PRODUCTO)
      INTO i_producto
      FROM PRECIOS_OPI PRE
      WHERE PRE.PAIS=p_pais
            AND PRE.TARIFA=p_tarifa
            AND PRE.PRODUCTO=p_producto;

      IF (i_producto = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_ExistPrecio_Articulos_o;

  PROCEDURE CARGA_PRECIOS_ARTICULOS IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de Precios_articulos a Precios
    -- ----------------------------------------

    --     v_PVP_anterior NUMBER;
    --     v_REBAJA1_anterior NUMBER;
    --     v_REBAJA2_anterior NUMBER;
    --     v_REMATE_anterior NUMBER;
    --     v_PVP_ORIGINAL_anterior NUMBER;
    --     v_OPI_anterior VARCHAR2(1);

    existeix char(1);

    CURSOR c_precio_articulo IS
      select pre.*,
        NVL(prec.PVP,0) as v_PVP_anterior,
        NVL(prec.REBAJA1,0) as v_REBAJA1_anterior,
        NVL(prec.REBAJA2,0) as v_REBAJA2_anterior,
        NVL(prec.REMATE,0) as v_REMATE_anterior,
        NVL(prec.PVP_ORIGINAL,0) as v_PVP_ORIGINAL_anterior,
        prec.OPI as v_OPI_anterior,
        NVL(prec.PRODUCTO,'NOTFOUND') as prec_producto
      from
        (
          SELECT distinct PA.ID as PAIS,
            PRE.PRODUCTO,
                          PRE.PVP/100 AS PVP_PRECIO,PRE.REBAJA1/100 AS REBAJA1,PRE.REBAJA2/100 AS REBAJA2,PRE.REMATE/100 AS REMATE,PRE.DIVISA,PRE.TARIFA,PRE.FECHA,PRE.PVP_ORIGINAL/100 AS PVP_ORIGINAL,
                          DECODE(TRIM(PRE.IVA),'',0,'I',1,PRE.IVA) AS TIPO_IVA,
            PRE.OPI
          FROM MNGDES2.MNG_PRECIOS PRE, PAISES PA
          WHERE PA.TARIFA=PRE.TARIFA -- Tarifas con las que trabajamos en cada pais
        ) pre, PRECIOS prec
      where  prec.PRODUCTO (+) = PRE.PRODUCTO and
             prec.PAIS (+) = PRE.PAIS AND
             prec.TARIFA (+) = PRE.TARIFA
      ORDER BY pre.producto;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PRECIOS_ARTICULOS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PRECIOS_ARTICULOS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    ltimestamp_work         timestamp;
    max_segons_avis_trace   number := 2;
    cal_generar_trace       char(1);

    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);

    num_regs                number;

    num_inserts             number;
    num_inserts2            number;
    num_updates             number;
    num_deletes             number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      num_inserts := 0;
      num_updates := 0;

      select PCKG_EXPLOTACIO.CAL_GENERAR_TRACE(id_proc,sub_id_proc) into cal_generar_trace from dual;

      if cal_generar_trace = 'S' then
        PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_begin,systimestamp,'Inici');
        ltimestamp_work := systimestamp;
      end if;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Trunco TMP_CAMBIOS...');

      --Trabajamos con la tabla tmp_cambios_carga_precios para calcular cuantos productos y cuales han cambiado con la nueva carga.
      execute immediate 'TRUNCATE TABLE tmp_cambios_carga_precios';

      if cal_generar_trace = 'S' then
        PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_begin,systimestamp,'Obro Cursor');
        ltimestamp_work := systimestamp;
      end if;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PRECIOS');

      FOR v_c_precio_articulo IN c_precio_articulo LOOP

        num_regs := num_regs + 1;

        if v_c_precio_articulo.prec_producto = 'NOTFOUND' then
          --            if existeix = 'N' then
          --Insertamos el precio en la tabla PRECIOS porque no existe

          BEGIN
            num_inserts := num_inserts + 1;

            INSERT INTO PRECIOS
            (PAIS,PRODUCTO,PVP,REBAJA1,REBAJA2,REMATE,DIVISA,TARIFA,FECHA,PVP_ORIGINAL,TIPO,OPI)
            VALUES    (v_c_precio_articulo.PAIS,
              v_c_precio_articulo.PRODUCTO,
              v_c_precio_articulo.PVP_PRECIO,
              v_c_precio_articulo.REBAJA1,
              v_c_precio_articulo.REBAJA2,
              v_c_precio_articulo.REMATE,
              v_c_precio_articulo.DIVISA,
              v_c_precio_articulo.TARIFA,
              v_c_precio_articulo.FECHA,
              v_c_precio_articulo.PVP_ORIGINAL,
              v_c_precio_articulo.TIPO_IVA,
                       v_c_precio_articulo.OPI
            );

            EXCEPTION
            WHEN OTHERS THEN
            ANOTAR_TRAZA('ERROR1:'||v_c_precio_articulo.PRODUCTO||'|'||v_c_precio_articulo.PVP_PRECIO||'|'||v_c_precio_articulo.REBAJA1||'|'||v_c_precio_articulo.REBAJA2||'|'||v_c_precio_articulo.REMATE||'|'||v_c_precio_articulo.TIPO_IVA||'|','CARGADATOS_MNG".CARAGA_PRECIOS_ARTICULOS',60);
          END;

        ELSE
          --Hacemos un update de los campos

          /* KIKU 22/1/2014 Optimització per no fer aquesta select 2 cops (a BUSCA_EXISTPRECIO_ARTICULOS i aquí)

                 select NVL(pre.PVP,0), NVL(pre.REBAJA1,0), NVL(pre.REBAJA2,0), NVL(pre.REMATE,0), NVL(pre.PVP_ORIGINAL,0) ,OPI
                 into
                    v_PVP_anterior,
                    v_REBAJA1_anterior,
                    v_REBAJA2_anterior,
                    v_REMATE_anterior,
                    v_PVP_ORIGINAL_anterior,
                    v_OPI_anterior
                 from precios pre
                 where
                    pre.PRODUCTO=v_c_precio_articulo.PRODUCTO and
                    pre.PAIS= v_c_precio_articulo.PAIS AND
                    pre.TARIFA =v_c_precio_articulo.TARIFA;
*/

          IF((v_c_precio_articulo.v_OPI_anterior is null OR v_c_precio_articulo.v_OPI_anterior!= v_c_precio_articulo.OPI) OR
             (v_c_precio_articulo.v_PVP_anterior != v_c_precio_articulo.PVP_PRECIO) OR
             (v_c_precio_articulo.v_REBAJA1_anterior != v_c_precio_articulo.REBAJA1) OR
             (v_c_precio_articulo.v_REBAJA2_anterior != v_c_precio_articulo.REBAJA2) OR
             (v_c_precio_articulo.v_REMATE_anterior != v_c_precio_articulo.REMATE) OR
             (v_c_precio_articulo.v_PVP_ORIGINAL_anterior != v_c_precio_articulo.PVP_ORIGINAL))
          THEN -- Si el precio anterior es diferente del nuevo  o la marca OPI es diferente al anterio hacemos un update de la tabla PRECIOS asi como actualizamos el campo en la tabla tmp_cambios_carga_precios

            IF((v_c_precio_articulo.v_PVP_anterior!=0) AND (v_c_precio_articulo.v_PVP_anterior != v_c_precio_articulo.PVP_PRECIO)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'PVP', v_c_precio_articulo.v_PVP_anterior, v_c_precio_articulo.PVP_PRECIO, SYSDATE);
            END IF;

            IF((v_c_precio_articulo.v_REBAJA1_anterior != 0) AND (v_c_precio_articulo.v_REBAJA1_anterior != v_c_precio_articulo.REBAJA1)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REBAJA1', v_c_precio_articulo.v_REBAJA1_anterior, v_c_precio_articulo.REBAJA1, SYSDATE);
            END IF;

            IF((v_c_precio_articulo.v_REBAJA2_anterior != 0) AND (v_c_precio_articulo.v_REBAJA2_anterior != v_c_precio_articulo.REBAJA2)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REBAJA2', v_c_precio_articulo.v_REBAJA2_anterior, v_c_precio_articulo.REBAJA2, SYSDATE);
            END IF;

            IF((v_c_precio_articulo.v_REMATE_anterior != 0) AND (v_c_precio_articulo.v_REMATE_anterior != v_c_precio_articulo.REMATE)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REMATE', v_c_precio_articulo.v_REMATE_anterior, v_c_precio_articulo.REMATE, SYSDATE);
            END IF;

            IF((v_c_precio_articulo.v_PVP_ORIGINAL_anterior !=0 ) AND (v_c_precio_articulo.v_PVP_ORIGINAL_anterior != v_c_precio_articulo.PVP_ORIGINAL)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'PVP_ORIGINAL', v_c_precio_articulo.v_PVP_ORIGINAL_anterior, v_c_precio_articulo.PVP_ORIGINAL, SYSDATE);
            END IF;


            BEGIN

              num_updates := num_updates + 1;

              UPDATE PRECIOS SET
                PVP = v_c_precio_articulo.PVP_PRECIO,
                REBAJA1 = v_c_precio_articulo.REBAJA1,
                REBAJA2 = v_c_precio_articulo.REBAJA2,
                REMATE = v_c_precio_articulo.REMATE,
                DIVISA = v_c_precio_articulo.DIVISA,
                TARIFA = v_c_precio_articulo.TARIFA,
                FECHA = v_c_precio_articulo.FECHA,
                PVP_ORIGINAL = v_c_precio_articulo.PVP_ORIGINAL,
                TIPO = v_c_precio_articulo.TIPO_IVA,
                OPI= v_c_precio_articulo.OPI
              WHERE PAIS = v_c_precio_articulo.PAIS
                    AND PRODUCTO = v_c_precio_articulo.PRODUCTO
                    AND TARIFA = v_c_precio_articulo.TARIFA;

              EXCEPTION
              WHEN OTHERS THEN
              ANOTAR_TRAZA ('ERROR2:'||v_c_precio_articulo.PRODUCTO||'|'||v_c_precio_articulo.PVP_PRECIO||'|'||v_c_precio_articulo.REBAJA1||'|'||v_c_precio_articulo.REBAJA2||'|'||v_c_precio_articulo.REMATE||'|'||v_c_precio_articulo.TIPO_IVA||'|','CARGADATOS_MNG2.CARGA_PRECIOS_ARTICULOS',60);
            END;
          END IF;
        END IF;
      END LOOP;

      -- KIKU 20/1/2014 Sols faig un commit
      COMMIT;

      -- ACT_TMP_PRECIOS;

      -- actualizamos los precios del esquema web

      /*
      PCKG_EXPLOTACIO.SET_SESSION_ACTION('Delete WEB.PRECIOS');
      if cal_generar_trace = 'S' then
          PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_work,systimestamp,'Abans DELETE WEB.PRECIOS');
          ltimestamp_work := systimestamp;
      end if;

      DELETE WEB.PRECIOS pr where substr(pr.producto,0,1) in (select distinct substr(p.producto,0,1) from mngdes2.mng_precios p );
      num_deletes := sql%rowcount;

      -- KIKU 22/1/2014 Trec aquest commit pq si el delete funciona però el insert posterior no... deixariem la taula sense contingut!
      --COMMIT;

      PCKG_EXPLOTACIO.SET_SESSION_ACTION('Insert WEB.PRECIOS');
      if cal_generar_trace = 'S' then
          PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_work,systimestamp,'Abans INSERT WEB.PRECIOS');
          ltimestamp_work := systimestamp;
      end if;

      insert into web.precios
         select pais, producto, pvp/100 as pvp, rebaja1/100 as  rebaja1, rebaja2/100 as rebaja2, remate/100 as remate, divisa, tarifa, fecha, fecha_carga
           from mngdes2.mng_precios p
          where p.pais <> '624'; -- Se excluye el pais Israel por peticion expresa del franquiciado.

      num_inserts2 := sql%rowcount;

*/

      -- 13/2/2014 En comptes de fer DELETE & INSERT, Faig un MERGE

      -- 17/9/2018 Sembla que no hi ha cap PHP que accedeixi a la taula WEB.PRECIOS
      --           Així que deixem d'actualitzar-la

      num_deletes := sql%rowcount;
      
--      PCKG_EXPLOTACIO.SET_SESSION_ACTION('Merge WEB.PRECIOS');
--      if cal_generar_trace = 'S' then
--        PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_work,systimestamp,'Abans MERGE WEB.PRECIOS');
--        ltimestamp_work := systimestamp;
--      end if;
--
--      merge into web.precios t1
--      using
--        (select pais, producto, pvp/100 as pvp, rebaja1/100 as rebaja1, rebaja2/100 as rebaja2, remate/100 as remate, divisa, tarifa, fecha, fecha_carga
--         from mngdes2.mng_precios p
--         where p.pais <> '624') t
--      ON (t.producto = t1.producto and t.pais = t1.pais and t.tarifa = t1.tarifa)
--      WHEN not MATCHED THEN
--      INSERT
--      (pais, producto, pvp, rebaja1, rebaja2, remate, divisa, tarifa, fecha, fecha_carga)
--      VALUES
--        (t.pais, t.producto, t.pvp, t.rebaja1, t.rebaja2, t.remate, t.divisa, t.tarifa, t.fecha, t.fecha_carga)
--      WHEN MATCHED THEN
--      UPDATE
--      SET pvp = t.pvp, rebaja1 = t.rebaja1, rebaja2 = t.rebaja2, remate = t.remate, divisa = t.divisa, fecha = t.fecha, fecha_carga = t.fecha_carga;
--
--      num_deletes := sql%rowcount;


      --QUITAR EL 28/07   !!!!!!!!!!!
      --update precios set rebaja1=0, rebaja2=0, remate=0 where pais = '001' and producto in ('41000363','41100153','45100560','45100786','43334150','45235025','41234161','41234351','43234155','43234159','45234152','45234553','45234555','41214150','41214151','41213150','41213355','41213650','43213153','46213776','46213777','41201658','41200165','41200371','41200788','41200925','43200182','43200188','43200190','43200782','45200368','46200195','49200032','49200155','41208064','41208090','41208699','45208038','45208039','49208211','49208506','49212058','49212059','49212062','49212064','41106776','43106351','45106151','41103156','43103150','49103354','41003125','43309150','49309156','43308162','43210025','43210086','43210263','45210084','45342031','41313350','43313151','41312166','41312171','45312174','45312177','45312180','45312558','45312791','45312794','49312040','49312163','49312185','49312283','49312926','43229026','41328001','41328148','41328457','41928082','41928083','45328079','49328276','49328278','49206250','45205651','49015550','41440126','41440151','41439150','41439363','41439364','43439160','45439575','46439563','49439374','43441030','49441251','49441658','49208919','43439351','43212276','43439553','43208238','43207650','43208660','41439158','41440161','43207453','43200164','43208302','43210251','43208300','43441028','43212044','45312181','46328282','41234358','41200180','43200154','41312399','43439552','43312550','43200361','45210450','43104151','45439929','43212273','43440551','46328283','49439166','41201153','43441900','41439165','45212452','41440166','45200176','43441652','43103350','43210258','42441901','41328147','41313450','41201126','43000162','43312190','43441250','43200158');

      if cal_generar_trace = 'S' then
        PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_work,systimestamp,'Abans COMMIT');
        ltimestamp_work := systimestamp;
      end if;

      COMMIT;

      if cal_generar_trace = 'S' then
        PCKG_EXPLOTACIO.ESCRIU_TRACE(id_proc, sub_id_proc, ltimestamp_begin, ltimestamp_work,systimestamp,'Final');
      end if;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Regs ('|| to_char(num_inserts) ||' Inserts, '||to_char(num_updates)||' Updates, '||to_char(num_deletes)||' Merge)';


      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_PRECIOS_ARTICULOS;


  PROCEDURE CARGA_PRECIOS_ARTICULOS_OPI IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de Precios_articulos a Precios
    -- ----------------------------------------
    v_PVP_anterior NUMBER;
    v_REBAJA1_anterior NUMBER;
    v_REBAJA2_anterior NUMBER;
    v_REMATE_anterior NUMBER;
    v_PVP_ORIGINAL_anterior NUMBER;
    v_OPI_anterior VARCHAR2(1);

    CURSOR c_precio_articulo IS
      SELECT distinct PA.ID as PAIS,
        PRE.PRODUCTO,
                      PRE.PVP/100 AS PVP_PRECIO,
                      PRE.REBAJA1/100 AS REBAJA1,
                      PRE.REBAJA2/100 AS REBAJA2,
                      PRE.REMATE/100 AS REMATE,
        PRE.DIVISA,
        PRE.TARIFA,
        PRE.FECHA,
                      PRE.PVP_ORIGINAL/100 AS PVP_ORIGINAL,
                      DECODE(TRIM(PRE.IVA),'',0,PRE.IVA) AS TIPO_IVA,
        PRE.OPI
      FROM MNGDES2.MNG_PRECIOS_OPI PRE, PAISES PA
      WHERE PA.TARIFA=PRE.TARIFA -- Tarifas con las que trabajamos en cada pais
            AND (PA.VENTA='S')
      ORDER BY producto;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PRECIOS_ARTICULOS_OPI';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PRECIOS_ARTICULOS_OPI';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      --Trabajamos con la tabla tmp_cambios_carga_precios para calcular cuantos productos y cuales han cambiado con la nueva carga.
      execute immediate 'TRUNCATE TABLE tmp_cambios_carga_precios';

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PRECIOS_OPI');

      FOR v_c_precio_articulo IN c_precio_articulo
      LOOP
        num_regs := num_regs + 1;

        IF(Busca_ExistPrecio_Articulos_o(v_c_precio_articulo.PRODUCTO, v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA)=0)THEN
          --Insertamos el precio en la tabla PRECIOS porque no existe

          BEGIN

            INSERT INTO PRECIOS_OPI
            (PAIS,
             PRODUCTO,
             PVP,
             REBAJA1,
             REBAJA2,
             REMATE,
             DIVISA,
             TARIFA,
             FECHA,
             PVP_ORIGINAL,
             TIPO,
             OPI
            )
            VALUES    (v_c_precio_articulo.PAIS,
              v_c_precio_articulo.PRODUCTO,
              v_c_precio_articulo.PVP_PRECIO,
              v_c_precio_articulo.REBAJA1,
              v_c_precio_articulo.REBAJA2,
              v_c_precio_articulo.REMATE,
              v_c_precio_articulo.DIVISA,
              v_c_precio_articulo.TARIFA,
              v_c_precio_articulo.FECHA,
              v_c_precio_articulo.PVP_ORIGINAL,
              v_c_precio_articulo.TIPO_IVA,
                       v_c_precio_articulo.OPI
            );

            EXCEPTION
            WHEN OTHERS THEN
            anotar_TRAZA('ERROR1:'||v_c_precio_articulo.PRODUCTO||'|'||v_c_precio_articulo.PVP_PRECIO||'|'||v_c_precio_articulo.REBAJA1||'|'||v_c_precio_articulo.REBAJA2||'|'||v_c_precio_articulo.REMATE||'|'||v_c_precio_articulo.TIPO_IVA||'|','CARGADATOS_MNG2.CARGA_PRECIOS_ARTICULOS',60);
          END;

        ELSE
          --Hacemos un update de los campos
          select NVL(pre.PVP,0), NVL(pre.REBAJA1,0), NVL(pre.REBAJA2,0), NVL(pre.REMATE,0), NVL(pre.PVP_ORIGINAL,0) ,PRE.OPI
          into
            v_PVP_anterior,
            v_REBAJA1_anterior,
            v_REBAJA2_anterior,
            v_REMATE_anterior,
            v_PVP_ORIGINAL_anterior,
            v_OPI_anterior
          from PRECIOS_OPI pre
          where
            pre.PRODUCTO=v_c_precio_articulo.PRODUCTO and
            pre.PAIS= v_c_precio_articulo.PAIS AND
            pre.TARIFA =v_c_precio_articulo.TARIFA;

          IF ((v_OPI_anterior is null OR v_OPI_anterior!= v_c_precio_articulo.OPI) OR (v_PVP_anterior!=v_c_precio_articulo.PVP_PRECIO)OR(v_REBAJA1_anterior!=v_c_precio_articulo.REBAJA1)OR(v_REBAJA2_anterior!=v_c_precio_articulo.REBAJA2)OR(v_REMATE_anterior!=v_c_precio_articulo.REMATE)OR(v_PVP_ORIGINAL_anterior!=v_c_precio_articulo.PVP_ORIGINAL))THEN -- Si el precio anterior es diferente del nuevo  o la marca OPI es diferente al anterio hacemos un update de la tabla PRECIOS asi como actualizamos el campo en la tabla tmp_cambios_carga_precios

            IF ((v_PVP_anterior!=0) AND (v_PVP_anterior!=v_c_precio_articulo.PVP_PRECIO)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'PVP', v_PVP_anterior, v_c_precio_articulo.PVP_PRECIO, SYSDATE);
            END IF;

            IF ((v_REBAJA1_anterior!=0) AND (v_REBAJA1_anterior!=v_c_precio_articulo.REBAJA1)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REBAJA1', v_REBAJA1_anterior, v_c_precio_articulo.REBAJA1, SYSDATE);
            END IF;

            IF ((v_REBAJA2_anterior!=0) AND (v_REBAJA2_anterior!=v_c_precio_articulo.REBAJA2)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REBAJA2', v_REBAJA2_anterior, v_c_precio_articulo.REBAJA2, SYSDATE);
            END IF;

            IF ((v_REMATE_anterior!=0) AND (v_REMATE_anterior!=v_c_precio_articulo.REMATE)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'REMATE', v_REMATE_anterior, v_c_precio_articulo.REMATE, SYSDATE);
            END IF;

            IF ((v_PVP_ORIGINAL_anterior!=0) AND (v_PVP_ORIGINAL_anterior!=v_c_precio_articulo.PVP_ORIGINAL)) THEN
              INSERT INTO tmp_cambios_carga_precios VALUES (v_c_precio_articulo.PAIS, v_c_precio_articulo.TARIFA, v_c_precio_articulo.DIVISA, v_c_precio_articulo.PRODUCTO, 'PVP_ORIGINAL', v_PVP_ORIGINAL_anterior, v_c_precio_articulo.PVP_ORIGINAL, SYSDATE);
            END IF;

            BEGIN

              UPDATE PRECIOS_OPI SET
                PVP = v_c_precio_articulo.PVP_PRECIO,
                REBAJA1 = v_c_precio_articulo.REBAJA1,
                REBAJA2 = v_c_precio_articulo.REBAJA2,
                REMATE = v_c_precio_articulo.REMATE,
                DIVISA = v_c_precio_articulo.DIVISA,
                TARIFA = v_c_precio_articulo.TARIFA,
                FECHA = v_c_precio_articulo.FECHA,
                PVP_ORIGINAL = v_c_precio_articulo.PVP_ORIGINAL,
                TIPO = v_c_precio_articulo.TIPO_IVA ,
                OPI= v_c_precio_articulo.OPI
              WHERE PAIS = v_c_precio_articulo.PAIS
                    AND PRODUCTO = v_c_precio_articulo.PRODUCTO
                    AND TARIFA = v_c_precio_articulo.TARIFA;

              EXCEPTION
              WHEN OTHERS THEN
              anotar_TRAZA ('ERROR2:'||v_c_precio_articulo.PRODUCTO||'|'||v_c_precio_articulo.PVP_PRECIO||'|'||v_c_precio_articulo.REBAJA1||'|'||v_c_precio_articulo.REBAJA2||'|'||v_c_precio_articulo.REMATE||'|'||v_c_precio_articulo.TIPO_IVA||'|','CARGADATOS_MNG2.CARGA_PRECIOS_ARTICULOS',60);
            END;
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      -- ACT_TMP_PRECIOS;

--      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'DELETE WEB.PRECIOS');
--
--      -- actualizamos los precios del esquema web
--
--      DELETE WEB.PRECIOS pr where substr(pr.producto,0,1) in (select distinct substr(p.producto,0,1) from mngdes2.mng_precios p );
--
--      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'INSERT WEB.PRECIOS');
--
--      insert into web.precios select pais, producto, pvp/100 as pvp, rebaja1/100 as  rebaja1, rebaja2/100 as rebaja2, remate/100 as remate, divisa, tarifa, fecha, fecha_carga
--                              from mngdes2.mng_precios p
--                              where p.pais <> '624'; -- Se excluye el pais Israel por peticion expresa del franquiciado.

      --QUITAR EL 28/07   !!!!!!!!!!!
      --update precios set rebaja1=0, rebaja2=0, remate=0 where pais = '001' and producto in ('41000363','41100153','45100560','45100786','43334150','45235025','41234161','41234351','43234155','43234159','45234152','45234553','45234555','41214150','41214151','41213150','41213355','41213650','43213153','46213776','46213777','41201658','41200165','41200371','41200788','41200925','43200182','43200188','43200190','43200782','45200368','46200195','49200032','49200155','41208064','41208090','41208699','45208038','45208039','49208211','49208506','49212058','49212059','49212062','49212064','41106776','43106351','45106151','41103156','43103150','49103354','41003125','43309150','49309156','43308162','43210025','43210086','43210263','45210084','45342031','41313350','43313151','41312166','41312171','45312174','45312177','45312180','45312558','45312791','45312794','49312040','49312163','49312185','49312283','49312926','43229026','41328001','41328148','41328457','41928082','41928083','45328079','49328276','49328278','49206250','45205651','49015550','41440126','41440151','41439150','41439363','41439364','43439160','45439575','46439563','49439374','43441030','49441251','49441658','49208919','43439351','43212276','43439553','43208238','43207650','43208660','41439158','41440161','43207453','43200164','43208302','43210251','43208300','43441028','43212044','45312181','46328282','41234358','41200180','43200154','41312399','43439552','43312550','43200361','45210450','43104151','45439929','43212273','43440551','46328283','49439166','41201153','43441900','41439165','45212452','41440166','45200176','43441652','43103350','43210258','42441901','41328147','41313450','41201126','43000162','43312190','43441250','43200158');

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END Carga_Precios_Articulos_OPI;

  PROCEDURE CARGA_PROMOS_ARTICULOS IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de recios_promos a Precios_Promo
    -- ----------------------------------------
    CURSOR c_precio_promo_articulo IS
      SELECT PRO.PAIS,
        PRO.PRODUCTO,
        PRO.PVP/100 AS PVP_PROMO,
        PRO.ID AS TIPO_PROMO
      FROM MNGDES2.MNG_PROMOS PRO, PAISES PA
      WHERE PA.ID=PRO.PAIS --Paises a los que vendemos
            AND PA.VENTA='S'
      ORDER BY PRODUCTO;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PROMOS_ARTICULOS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PROMOS_ARTICULOS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);

    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PROMOS');

      FOR v_c_precio_promo_articulo IN c_precio_promo_articulo
      LOOP

        num_regs := num_regs + 1;

        BEGIN
          INSERT INTO PRECIOS_PROMOS
          (PAIS,
           PRODUCTO,
           PROMO,
           TIPO_PROMO)
          VALUES    (v_c_precio_promo_articulo.PAIS,
                     v_c_precio_promo_articulo.PRODUCTO,
                     v_c_precio_promo_articulo.PVP_PROMO,
                     v_c_precio_promo_articulo.TIPO_PROMO);

          EXCEPTION
          WHEN dup_val_on_index THEN --Entrará aqui cuando ya exista este registro por lo que realizaremos un update
          UPDATE PRECIOS_PROMOS SET
            PROMO = v_c_precio_promo_articulo.PVP_PROMO
          WHERE PAIS = v_c_precio_promo_articulo.PAIS
                AND PRODUCTO = v_c_precio_promo_articulo.PRODUCTO
                AND TIPO_PROMO = v_c_precio_promo_articulo.TIPO_PROMO;
        END;--del try(BEGIN-EXCEPTION)
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END Carga_Promos_Articulos;


  PROCEDURE CARGA_PROMOS_ARTICULOS_OPI IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de recios_promos a Precios_Promo
    -- ----------------------------------------
    CURSOR c_precio_promo_articulo IS
      SELECT PRO.PAIS,
        PRO.PRODUCTO,
        PRO.PVP/100 AS PVP_PROMO,
        PRO.ID AS TIPO_PROMO
      FROM MNGDES2.MNG_PROMOS PRO, PAISES PA
      WHERE PA.ID=PRO.PAIS --Paises a los que vendemos
            AND PA.VENTA='S'
      ORDER BY PRODUCTO;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_PROMOS_ARTICULOS_OPI';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PROMOS_ARTICULOS_OPI';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN

      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PROMOS');

      FOR v_c_precio_promo_articulo IN c_precio_promo_articulo
      LOOP
        num_regs := num_regs + 1;

        BEGIN
          INSERT INTO PRECIOS_PROMOS
          (PAIS,
           PRODUCTO,
           PROMO,
           TIPO_PROMO)
          VALUES    (v_c_precio_promo_articulo.PAIS,
                     v_c_precio_promo_articulo.PRODUCTO,
                     v_c_precio_promo_articulo.PVP_PROMO,
                     v_c_precio_promo_articulo.TIPO_PROMO);
          EXCEPTION
          WHEN dup_val_on_index THEN --Entrará aqui cuando ya exista este registro por lo que realizaremos un update
          UPDATE PRECIOS_PROMOS SET
            PROMO = v_c_precio_promo_articulo.PVP_PROMO
          WHERE PAIS = v_c_precio_promo_articulo.PAIS
                AND PRODUCTO = v_c_precio_promo_articulo.PRODUCTO
                AND TIPO_PROMO = v_c_precio_promo_articulo.TIPO_PROMO;
        END;--del try(BEGIN-EXCEPTION)
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END Carga_Promos_Articulos_OPI;

  PROCEDURE Carga_Precios_Articulos_000 IS
    -- ----------------------------------------
    -- ESTO ES UN PARCHE TEMPORAL
    -- Descripcion: Procedimiento de migracion
    --              de datos de Productos y Rebajas a Precios_Promo
    --                para temporalmente conseguir los precios de productos y rebajas guardandolos como el pais 000
    -- ----------------------------------------
    CURSOR c_precio_articulo IS
      SELECT '000' AS PAIS,
             PRO.ID AS PRODUCTO,
             REPLACE(PRO.PVP,'.',',') AS PVP_PRECIO,
             NVL(REPLACE(RE1.PRECIO,'.',','),'0') AS REBAJA1,
             NVL(REPLACE(RE2.PRECIO,'.',','),'0') AS REBAJA2,
             NVL(REPLACE(REM.PRECIO,'.',','),'0') AS REMATE,
             'EUR' AS DIVISA,
             '00' AS TARIFA,
             TO_CHAR(SYSDATE,'YYYYMMDD') AS FECHA
      FROM PRODUCTOS PRO, REBAJAS RE1, REBAJAS RE2, REBAJAS REM












      WHERE PRO.ID =  RE1.PRODUCTO AND RE1.TIPO=1 AND
            PRO.ID =  RE2.PRODUCTO AND RE2.TIPO=2 AND
            PRO.ID =  REM.PRODUCTO AND REM.TIPO=3 AND
            PRO.ID LIKE '1%' --TEMPORADA
      ORDER BY PRO.ID;
    BEGIN

      FOR v_c_precio_articulo IN c_precio_articulo
      LOOP

        BEGIN
          INSERT INTO PRECIOS
          (PAIS,
           PRODUCTO,
           PVP,
           REBAJA1,
           REBAJA2,
           REMATE,
           DIVISA,
           TARIFA,
           FECHA)
          VALUES    (v_c_precio_articulo.PAIS,
                     v_c_precio_articulo.PRODUCTO,
                     v_c_precio_articulo.PVP_PRECIO,
                     v_c_precio_articulo.REBAJA1,
                     v_c_precio_articulo.REBAJA2,
                     v_c_precio_articulo.REMATE,
                     v_c_precio_articulo.DIVISA,
                     v_c_precio_articulo.TARIFA,
                     v_c_precio_articulo.FECHA);
          EXCEPTION
          WHEN dup_val_on_index THEN --Entrará aqui cuando ya exista este registro por lo que realizaremos un update
          UPDATE PRECIOS SET
            PVP = v_c_precio_articulo.PVP_PRECIO,
            REBAJA1 = v_c_precio_articulo.REBAJA1,
            REBAJA2 = v_c_precio_articulo.REBAJA2,
            REMATE = v_c_precio_articulo.REMATE,
            DIVISA = v_c_precio_articulo.DIVISA,
            TARIFA = v_c_precio_articulo.TARIFA,
            FECHA = v_c_precio_articulo.FECHA
          WHERE PAIS = v_c_precio_articulo.PAIS
                AND PRODUCTO = v_c_precio_articulo.PRODUCTO;
        END;--del try(BEGIN-EXCEPTION)
      END LOOP;

      COMMIT;

    END Carga_Precios_Articulos_000;

  PROCEDURE CARGA_DIVISAS IS
    ----------------------------------------
    -- Descripcion: Procedimiento de carga de cambios de divisas
    ------------------------------------------
    CURSOR c_divisas IS
      SELECT *
      FROM MNGDES2.MNG_DIVISAS;

    ----------------------------------- FrameWork Explotació -----------------------------------
    id_proc                 CONSTANT varchar2(30) := 'CARGA_DIVISAS';
    sub_id_proc             CONSTANT varchar2(30) := 'CARGA_DIVISAS';
    -- Altres Variables
    permis_execucio         char(1);
    ltimestamp_begin        timestamp;
    ltimestamp_end          timestamp;
    status_exe_proc         varchar2(2048);
    parametres              varchar2(4000);
    num_regs                number;
    ----------------------------------- FrameWork Explotació -----------------------------------

    BEGIN
      -- Framework (Hora Inici Proc)
      ltimestamp_begin := systimestamp;

      -- Framework (Omplo String Parametres Crida Proc)
      parametres := '';
      num_regs := 0;

      PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_DIVISAS');

      FOR v_c_divisas IN c_divisas
      LOOP
        num_regs := num_regs + 1;

        BEGIN
          INSERT INTO rates_divisas
          (DIVISA,
           FECHA,
           VALOR)
          VALUES    (v_c_divisas.DIVISA,
                     v_c_divisas.FECHA,
                     v_c_divisas.VALOR);
          EXCEPTION
          WHEN dup_val_on_index THEN --Entrará aqui cuando ya exista este registro por lo que realizaremos un update
          UPDATE RATES_DIVISAS SET
            DIVISA = v_c_divisas.DIVISA,
            FECHA = v_c_divisas.FECHA,
            VALOR = v_c_divisas.VALOR
          WHERE DIVISA = v_c_divisas.DIVISA;
        END;--del try(BEGIN-EXCEPTION)
      END LOOP;

      COMMIT;

      -- Genero Logging OK
      status_exe_proc := to_char(num_regs)||' Registres';

      ltimestamp_end := systimestamp;
      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      EXCEPTION
      WHEN OTHERS THEN
      -- Netejo informació a v$session
      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      -- Genero Logging NOT OK!!!
      status_exe_proc := 'Error SQL:'||SQLCODE||' -> '||SQLERRM;
      ltimestamp_end := systimestamp;

      PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'E',status_exe_proc,parametres);

    END CARGA_DIVISAS;

  FUNCTION getArticuloBase (articulo IN VARCHAR2) RETURN varchar2 IS
    -- ------------------------------------------------
    --
    -- Dado un articulo, devuelve su id con la referencia BASE del producto
    --
    ---------------------------------------------------
    producto varchar2(8);
    productoBase varchar2(8);
    tallaColor varchar2(4);
    articuloBase varchar2(12);

    BEGIN
      SELECT substr(articulo,0,8) INTO producto FROM dual;

      SELECT substr(articulo,9,12) INTO tallaColor FROM dual;

      select base into productoBase from productos where id = producto;

      articuloBase := productoBase||tallaColor;

      return articuloBase;

    END getArticuloBase;

  PROCEDURE Carga_Faltas_Picking_001 IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking  EUROPA.
    -- ----------------------------------------
    fecha date;

    CURSOR c_faltaspicking IS
      SELECT *
      FROM MNGDES2.MNG_FALTAS_PICKING
      WHERE CLIENTE = '001' -- Por ahora sólo aplicamos los pedidos de sustitución en USA
      ORDER BY PEDIDO;

    BEGIN
      fecha := sysdate;

      FOR v_c_faltaspicking IN c_faltaspicking
      LOOP

        IF (v_c_faltaspicking.ESTADO = 0) THEN -- Si no existe stock del item actualizamos fecha_baja de lineas_pedido con sysdate
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = fecha
          WHERE PEDIDO = substr(v_c_faltaspicking.PEDIDO,1,6)
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido

          UPDATE PEDIDOS SET RES_BANCO = '1'
          WHERE ID =  substr(v_c_faltaspicking.PEDIDO,1,6) -- Actualizamos estado del pedido a 'pendiente revisión stock'
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00';
        END IF;

        IF (v_c_faltaspicking.ESTADO = 1) THEN -- Si actualizamos fecha_baja por si es 2a vez y hay alguna prenda que ahora si que hay
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = null
          WHERE PEDIDO = substr(v_c_faltaspicking.PEDIDO,1,6) and fecha <> fecha_baja
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
        END IF;
      END LOOP;

      COMMIT;

    END Carga_Faltas_Picking_001;

  PROCEDURE Carga_Faltas_Picking_400 IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking USA.
    -- ----------------------------------------
    fecha date;

    CURSOR c_faltaspicking IS
      SELECT *
      FROM MNGDES2.MNG_FALTAS_PICKING
      WHERE CLIENTE = '400' -- Por ahora sólo aplicamos los pedidos de sustitución en USA
      ORDER BY PEDIDO;

    BEGIN
      fecha := sysdate;

      FOR v_c_faltaspicking IN c_faltaspicking
      LOOP
        IF (v_c_faltaspicking.ESTADO = 0) THEN -- Si no existe stock del item actualizamos fecha_baja de lineas_pedido con sysdate
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = fecha
          WHERE PEDIDO = '40'||substr(v_c_faltaspicking.PEDIDO,1,6)
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido

          UPDATE PEDIDOS SET RES_BANCO = '1'
          WHERE ID =  '40'||substr(v_c_faltaspicking.PEDIDO,1,6) -- Actualizamos estado del pedido a 'pendiente revisión stock'
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00';
        END IF;

        IF (v_c_faltaspicking.ESTADO = 1) THEN -- Si actualizamos fecha_baja por si es 2a vez y hay alguna prenda que ahora si que hay
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = null
          WHERE PEDIDO = '40'||substr(v_c_faltaspicking.PEDIDO,1,6) and fecha <> fecha_baja
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
        END IF;
      END LOOP;

      COMMIT;

    END Carga_Faltas_Picking_400;


  PROCEDURE Carga_Faltas_Picking_052 IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking TURQUIA.
    -- ----------------------------------------
    fecha date;

    CURSOR c_faltaspicking IS
      SELECT *
      FROM MNGDES2.MNG_FALTAS_PICKING
      WHERE CLIENTE = '052' -- Por ahora sólo aplicamos los pedidos de sustitución en USA
      ORDER BY PEDIDO;

    BEGIN
      fecha := sysdate;

      FOR v_c_faltaspicking IN c_faltaspicking
      LOOP
        IF (v_c_faltaspicking.ESTADO = 0) THEN -- Si no existe stock del item actualizamos fecha_baja de lineas_pedido con sysdate
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = fecha
          WHERE PEDIDO = 'TR'||substr(v_c_faltaspicking.PEDIDO,1,6)
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido

          UPDATE PEDIDOS SET RES_BANCO = '1'
          WHERE ID =  'TR'||substr(v_c_faltaspicking.PEDIDO,1,6) -- Actualizamos estado del pedido a 'pendiente revisión stock'
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00';
        END IF;

        IF (v_c_faltaspicking.ESTADO = 1) THEN -- Si actualizamos fecha_baja por si es 2a vez y hay alguna prenda que ahora si que hay
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = null
          WHERE PEDIDO = 'TR'||substr(v_c_faltaspicking.PEDIDO,1,6) and fecha <> fecha_baja
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
        END IF;
      END LOOP;

      COMMIT;

    END Carga_Faltas_Picking_052;

  PROCEDURE Carga_Faltas_Picking_720 IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking CHINA
    -- ----------------------------------------
    fecha date;

    CURSOR c_faltaspicking IS
      SELECT *
      FROM MNGDES2.MNG_FALTAS_PICKING
      WHERE CLIENTE = '720' -- Por ahora sólo aplicamos los pedidos de sustitución en USA
      ORDER BY PEDIDO;

    BEGIN
      fecha := sysdate;

      FOR v_c_faltaspicking IN c_faltaspicking
      LOOP
        IF (v_c_faltaspicking.ESTADO = 0) THEN -- Si no existe stock del item actualizamos fecha_baja de lineas_pedido con sysdate
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = fecha
          WHERE PEDIDO = 'CN'||substr(v_c_faltaspicking.PEDIDO,1,6)
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido

          UPDATE PEDIDOS SET RES_BANCO = '1'
          WHERE ID =  'CN'||substr(v_c_faltaspicking.PEDIDO,1,6) -- Actualizamos estado del pedido a 'pendiente revisión stock'
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00';
        END IF;

        IF (v_c_faltaspicking.ESTADO = 1) THEN -- Si actualizamos fecha_baja por si es 2a vez y hay alguna prenda que ahora si que hay
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = null
          WHERE PEDIDO = 'CN'||substr(v_c_faltaspicking.PEDIDO,1,6) and fecha <> fecha_baja
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
        END IF;
      END LOOP;

      COMMIT;

    END Carga_Faltas_Picking_720;

  PROCEDURE Carga_Faltas_Picking_075 IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking RUSIA
    -- ----------------------------------------
    fecha date;

    CURSOR c_faltaspicking IS
      SELECT *
      FROM MNGDES2.MNG_FALTAS_PICKING
      WHERE CLIENTE = '075' -- Por ahora sólo aplicamos los pedidos de sustitución en USA
      ORDER BY PEDIDO;

    BEGIN
      fecha := sysdate;

      FOR v_c_faltaspicking IN c_faltaspicking
      LOOP
        IF (v_c_faltaspicking.ESTADO = 0) THEN -- Si no existe stock del item actualizamos fecha_baja de lineas_pedido con sysdate
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = fecha
          WHERE PEDIDO = 'RU'||substr(v_c_faltaspicking.PEDIDO,1,6)
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido

          UPDATE PEDIDOS SET RES_BANCO = '1'
          WHERE ID =  'RU'||substr(v_c_faltaspicking.PEDIDO,1,6) -- Actualizamos estado del pedido a 'pendiente revisión stock'
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00';
        END IF;

        IF (v_c_faltaspicking.ESTADO = 1) THEN -- Si actualizamos fecha_baja por si es 2a vez y hay alguna prenda que ahora si que hay
          UPDATE LINEAS_PEDIDO SET FECHA_BAJA = null
          WHERE PEDIDO = 'RU'||substr(v_c_faltaspicking.PEDIDO,1,6) and fecha <> fecha_baja
                AND substr(v_c_faltaspicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
                AND ARTICULO = v_c_faltaspicking.ITEM|| v_c_faltaspicking.TALLA||v_c_faltaspicking.COLOR
                AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
        END IF;
      END LOOP;

      COMMIT;

    END Carga_Faltas_Picking_075;

  PROCEDURE Carga_Pagos IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de migracion
    --              de datos de picking.
    -- ----------------------------------------
    v_Tarjeta_Empleado NUMBER;

    CURSOR c_pagos IS
      SELECT *
      FROM MNGDES2.MNG_PAGOS
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_pagos IN c_pagos
      LOOP
        IF(v_c_pagos.CLIENTE = '052')THEN  -- Turquia
          UPDATE PEDIDOS SET PAGADO = 'P'
          WHERE ID = 'TR'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO = 'R';

          UPDATE PEDIDOS SET PAGADO = 'E'
          WHERE ID = 'TR'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO IS NULL;
        ELSIF(v_c_pagos.CLIENTE = '400')THEN -- EEUU
          UPDATE PEDIDOS SET PAGADO = 'P'
          WHERE ID = '40'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO = 'R';

          UPDATE PEDIDOS SET PAGADO = 'E'
          WHERE ID = '40'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO IS NULL;

        ELSIF(v_c_pagos.CLIENTE = '075')THEN -- Rusia
          UPDATE PEDIDOS SET PAGADO = 'P'
          WHERE ID = 'RU'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO = 'R';

          UPDATE PEDIDOS SET PAGADO = 'E'
          WHERE ID = 'RU'||substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO IS NULL;

        ELSIF(v_c_pagos.CLIENTE = '001')THEN -- Para Europa enviamos los emails para confirmar que el pedido ha sido recogido/pedido expedido
          UPDATE PEDIDOS SET PAGADO = 'P'
          WHERE ID = substr(v_c_pagos.PEDIDO,1,6)
                AND PAGADO = 'R';

          -- Marcamos en error pedidos de Falcon y PayPal
          UPDATE PEDIDOS SET PAGADO = 'E'
          WHERE ID = substr(v_c_pagos.PEDIDO,1,6) AND tipo_pago1 in ('F','P')
                AND PAGADO IS NULL;

          --Recogemos en la variable v_Tarjeta_Empleado_Pedido el tipo de tarjeta descuento
          SELECT count(*) INTO v_Tarjeta_Empleado FROM PEDIDOS_TARJETA_EMPLEADO pte where pte.PEDIDO=substr(v_c_pagos.PEDIDO,0,6);

          IF (v_Tarjeta_Empleado > 0) THEN
            SELECT substr(pte.TARJETA_EMPLEADO,0,1) INTO v_Tarjeta_Empleado FROM PEDIDOS_TARJETA_EMPLEADO pte where pte.PEDIDO=substr(v_c_pagos.PEDIDO,0,6);

            IF(v_Tarjeta_Empleado =7 OR v_Tarjeta_Empleado=8)THEN--Solo insertamos el PEDIDOS_MAILS los pedidos de tarjeta empleado
              BEGIN
                INSERT INTO PEDIDOS_MAILS (PEDIDO,TIPO_EMAIL,ENVIADO,FECHA_INSERT) VALUES(v_c_pagos.PEDIDO,'PEDIDO EXPEDIDO',0,SYSDATE);
                EXCEPTION
                WHEN dup_val_on_index THEN --Entrará aqui cuando ya exista este registro
                anotar_TRAZA('Valor duplicado al intentar insertar un registro [PEDIDO EXPEDIDO] en la tabla PEDIDOS_MAILS para el pedido '||v_c_pagos.PEDIDO,'CARGADATOS_MNG2.CARGA_PAGOS',60);
              END;--del try(BEGIN-EXCEPTION)
            END IF;
          END IF;
        END IF;
      END LOOP;

      COMMIT;

      EXCEPTION
      WHEN OTHERS THEN
      DBMS_OUTPUT.PUT_LINE(SQLERRM);
    END Carga_Pagos;

  FUNCTION Busca_Exist_Pedido_OK (p_id IN VARCHAR2) RETURN NUMBER IS
    -- ----------------------------------------
    -- Descripcion: Funcion de que nos dice si
    --                 existe ya el pedido en la tabla PEDIDOS_ESTADO
    -- ----------------------------------------
    i_Pedido_OK NUMBER := 0;

    BEGIN
      SELECT COUNT(*) INTO i_Pedido_OK FROM PEDIDOS_ESTADO WHERE ID = p_id;

      IF (i_Pedido_OK = 0) THEN
        RETURN 0;
      ELSE
        RETURN 1;
      END IF;

    END Busca_Exist_Pedido_OK;

PROCEDURE Carga_Pedidos_OK IS
-- ----------------------------------------
-- Descripcion: Procedimiento de copia de datos de los estados
--              de los pedidos despues de procesar en wintegrate.
-- ----------------------------------------
  v_existe NUMBER;

  ----------------------------------- FrameWork Explotació -----------------------------------
  id_proc                 CONSTANT varchar2(30) := 'CARGA_PEDIDOS_OK';
  sub_id_proc             CONSTANT varchar2(30) := 'CARGA_PEDIDOS_OK';
  -- Altres Variables
  permis_execucio         char(1);
  ltimestamp_begin        timestamp;
  ltimestamp_end          timestamp;
  status_exe_proc         varchar2(2048);
  parametres              varchar2(4000);
  num_regs                number;
  num_up                  number;
  ----------------------------------- FrameWork Explotació -----------------------------------

  CURSOR c_pedidos_ok IS
    SELECT ID,DECODE(ESTADO,'K','OK','E','ER','U','OK','--') ESTADO,MOTIVO,FECHA,ALMACEN
    FROM MNGDES2.MNG_PEDIDOS_OK MPO
    --WHERE MOTIVO IS NULL OR( MOTIVO NOT LIKE '%YA EXISTE EL PEDIDO%'
    --                           AND MOTIVO NOT LIKE '%YA ESTA ANULADO%'
    --                        AND MOTIVO NOT LIKE '%YA ESTA RETENIDO%')
    --carreguem el fitxer tal cual, encara que siguin errors de ja procesats
    WHERE ( ID LIKE '%00' OR (LENGTH(ID)=6 AND MPO.ESTADO='E' ));--no es subpedido o si el pedido tiene longitud 6 ( a veces llegan los errores mal desde COBOL)

  BEGIN
    -- Framework (Hora Inici Proc)
    ltimestamp_begin := systimestamp;

    -- Framework (Omplo String Parametres Crida Proc)
    parametres := '';
    num_regs := 0;

    PCKG_EXPLOTACIO.SET_SESSION_MODULE(id_proc, 'Cercant MNG_PEDIDOS_OK');

    FOR v_c_pedidos_ok IN c_pedidos_ok LOOP

      IF(v_c_pedidos_ok.ALMACEN = '400') THEN --USA

        v_existe := Busca_Exist_Pedido_OK('40'||SUBSTR(v_c_pedidos_ok.ID,0,6));

        IF (v_existe = 0) THEN --No existe
          --Insertamos los pedidos para USA (almacen 400)
          INSERT INTO PEDIDOS_ESTADO (ID, ESTADO, MOTIVO, FECHA) VALUES ('40'||SUBSTR(v_c_pedidos_ok.ID,0,6), v_c_pedidos_ok.ESTADO, v_c_pedidos_ok.MOTIVO, v_c_pedidos_ok.FECHA);
        ELSE
          --Updateamos los pedidos para USA (almacen 400)
          UPDATE PEDIDOS_ESTADO SET ESTADO=v_c_pedidos_ok.ESTADO, MOTIVO=v_c_pedidos_ok.MOTIVO WHERE ID='40'||SUBSTR(v_c_pedidos_ok.ID,0,6);
        END IF;

      ELSIF(v_c_pedidos_ok.ALMACEN = '052') THEN --Turquia

        v_existe := Busca_Exist_Pedido_OK('TR'||SUBSTR(v_c_pedidos_ok.ID,0,6));

        IF (v_existe = 0) THEN --No existe
          --Insertamos los pedidos para Turquia (almacen 052)
          INSERT INTO PEDIDOS_ESTADO (ID, ESTADO, MOTIVO, FECHA) VALUES ('TR'||SUBSTR(v_c_pedidos_ok.ID,0,6), v_c_pedidos_ok.ESTADO, v_c_pedidos_ok.MOTIVO, v_c_pedidos_ok.FECHA);
        ELSE
          --Updateamos los pedidos para Turquia (almacen 052)
          UPDATE PEDIDOS_ESTADO SET ESTADO=v_c_pedidos_ok.ESTADO, MOTIVO=v_c_pedidos_ok.MOTIVO WHERE ID='TR'||SUBSTR(v_c_pedidos_ok.ID,0,6);
        END IF;


      --------------------------------
      ELSIF(v_c_pedidos_ok.ALMACEN = '720') THEN --China

        v_existe := Busca_Exist_Pedido_OK('CN'||SUBSTR(v_c_pedidos_ok.ID,0,6));

        IF (v_existe = 0) THEN --No existe
          --Insertamos los pedidos para China (almacen 720)
          INSERT INTO PEDIDOS_ESTADO (ID, ESTADO, MOTIVO, FECHA) VALUES ('CN'||SUBSTR(v_c_pedidos_ok.ID,0,6), v_c_pedidos_ok.ESTADO, v_c_pedidos_ok.MOTIVO, v_c_pedidos_ok.FECHA);
        ELSE
          --Updateamos los pedidos para China (almacen 720)
          UPDATE PEDIDOS_ESTADO SET ESTADO=v_c_pedidos_ok.ESTADO, MOTIVO=v_c_pedidos_ok.MOTIVO WHERE ID='CN'||SUBSTR(v_c_pedidos_ok.ID,0,6);
        END IF;

      ELSIF(v_c_pedidos_ok.ALMACEN = '075') THEN --Rusia

        v_existe := Busca_Exist_Pedido_OK('RU'||SUBSTR(v_c_pedidos_ok.ID,0,6));

        IF (v_existe = 0) THEN --No existe
          --Insertamos los pedidos para Rusia (almacen 075)
          INSERT INTO PEDIDOS_ESTADO (ID, ESTADO, MOTIVO, FECHA) VALUES ('RU'||SUBSTR(v_c_pedidos_ok.ID,0,6), v_c_pedidos_ok.ESTADO, v_c_pedidos_ok.MOTIVO, v_c_pedidos_ok.FECHA);
        ELSE
          --Updateamos los pedidos para Rusia(almacen 075)
          UPDATE PEDIDOS_ESTADO SET ESTADO=v_c_pedidos_ok.ESTADO, MOTIVO=v_c_pedidos_ok.MOTIVO WHERE ID='RU'||SUBSTR(v_c_pedidos_ok.ID,0,6);
        END IF;

      ELSE
        v_existe := Busca_Exist_Pedido_OK(SUBSTR(v_c_pedidos_ok.ID,0,6));

        IF (v_existe = 0) THEN --No existe
          INSERT INTO PEDIDOS_ESTADO (ID, ESTADO, MOTIVO, FECHA) VALUES (SUBSTR(v_c_pedidos_ok.ID,0,6), v_c_pedidos_ok.ESTADO, v_c_pedidos_ok.MOTIVO, v_c_pedidos_ok.FECHA);
        ELSE
          UPDATE PEDIDOS_ESTADO SET ESTADO=v_c_pedidos_ok.ESTADO, MOTIVO=v_c_pedidos_ok.MOTIVO WHERE ID=SUBSTR(v_c_pedidos_ok.ID,0,6);
        END IF;

      END IF;

      num_regs := num_regs + 1;
    END LOOP;

    --un cop acabada la carrega, updategem com a OK tots els que tenen error tipus 'ya esta procesado'

    PCKG_EXPLOTACIO.SET_SESSION_ACTION('Updates PEDIDOS_ESTADO');

    -- *KIKU* 21/01/2015 En comptes de fer 7 UPDATES que llegiràn 7 vegades la taula... ho faig en una sola instrucció

   update pedidos_estado
    set motivo = case
                 when motivo like '%ESTA RETENIDO%' then 'PED.RT: RECIBIDO RETENIDO'
                 when motivo like '%ESTA ANULADO%' then 'CAN.CA: CANCELACION OK'
                 else 'PED.RO: RECIBIDO RETENIDO OK'
                 end,
      estado = 'OK'
    where fecha > SYSDATE - 5 and
          (motivo like '%ESTA RETENIDO%' or
           motivo like '%ESTA ANULADO%' or
           motivo like '%ESTA EXPEDIDO%' or
           motivo like '%ESTA ENCAJADO%' or
           motivo like '%YA EXISTE LA DEVOLUCION%' or
           motivo like '%YA ESTA FINALIZADO%' or
           motivo like '%YA EXISTE EL PEDIDO%');

    num_up := sql%rowcount;

    -- Genero Logging OK
    status_exe_proc := to_char(num_regs)||' Registres' ||
                       ' (Update:'||to_char(num_up)||')';

    ltimestamp_end := systimestamp;
    PCKG_EXPLOTACIO.ESCRIU_LOG_EXECUCIO (id_proc,sub_id_proc,ltimestamp_begin,ltimestamp_end,'O',status_exe_proc,parametres);

    -- Netejo informació a v$session
    PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

  END Carga_Pedidos_OK;

  PROCEDURE Carga_m4m IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento que actualiza el estado de los cupones cuando llegan los tickets desde el DWH
    -- ----------------------------------------
    BEGIN

      -- KIKU 2/6/2014 Per la migració a Amazon (que res del mango4mango es pasa), comento aquest codi
      --               Parlo amb Jordi Pereta i això és un desenvolupament antic... no s'hauria d'executar.

      PCKG_EXPLOTACIO.SET_SESSION_MODULE('', '');

      --           FOR filaTickets IN (SELECT codigo_cupon, fec_venta FROM mango4mango.m4m_tickets WHERE codigo_Cupon IS NOT NULL and fecha_act BETWEEN SYSDATE -1 AND SYSDATE )
      --           LOOP
      --                update mango4mango.cupones set estado=2, fecha_cangeo=filaTickets.fec_venta where codigo_cupon=filaTickets.codigo_cupon;
      --           END LOOP;
      --
      --           update mango4mango.cupones set estado=3 where fecha_gen<sysdate-372 and estado not in(2,3);
    END Carga_m4m;


  PROCEDURE Carga_TARJETA_EMPLEADOS IS
    -- ----------------------------------------
    -- Descripcion: Procedimiento de copia de datos acutalizados
    --              de las tarjetas de los empleados.
    -- ----------------------------------------
    v_contador NUMBER:=0;

    CURSOR c_tarjeta_empleados IS
      SELECT *
      FROM MNGDES2.MNG_TARJETA_EMPLEADOS;

    BEGIN

      FOR v_c_tarjeta_empleados IN c_tarjeta_empleados
      LOOP
        BEGIN
          IF(v_c_tarjeta_empleados.MOTIVO_ULT_CAMBIO = 'A')THEN--Tarjeta anulada (hay que borrarla)

            DELETE FROM TARJETA_EMPLEADOS WHERE CODIGO_CARNET=v_c_tarjeta_empleados.CODIGO_CARNET;

            --INSERT INTO TRAZA VALUES ('DELETE['||v_contador||':'||v_c_tarjeta_empleados.CODIGO_CARNET||']',SYSDATE);v_contador:=v_contador+1;

          ELSE -- Update o insert nueva información de la tarjeta

            UPDATE TARJETA_EMPLEADOS
            SET TIPO_DTO=v_c_tarjeta_empleados.TIPO_DTO,
              MOTIVO_ULT_CAMBIO=v_c_tarjeta_empleados.MOTIVO_ULT_CAMBIO,
              DTO_EN_LIMITE=v_c_tarjeta_empleados.DTO_EN_LIMITE,
              NOMBRE_EMPLEADO=NORMALIZAR_TEXTO(v_c_tarjeta_empleados.NOMBRE_EMPLEADO),
              LIMITE_S=TO_NUMBER(REPLACE(v_c_tarjeta_empleados.LIMITE_S,'+',''))/100,
              DNI_EMPLEADO=to_number(replace(translate(UPPER(v_c_tarjeta_empleados.DNI_EMPLEADO),'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ.-',' '),' ','')),
              DNI_FAMILIAR=to_number(replace(translate(UPPER(v_c_tarjeta_empleados.DNI_FAMILIAR),'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ.-',' '),' ','')),
              FECHA_ACT=SYSDATE
            WHERE CODIGO_CARNET=v_c_tarjeta_empleados.CODIGO_CARNET;

            --INSERT INTO TRAZA VALUES ('UPDATE['||v_contador||':'||v_c_tarjeta_empleados.CODIGO_CARNET||']',SYSDATE);v_contador:=v_contador+1;

            IF(SQL%NOTFOUND)THEN--Si el update anterior no se corresponde con ninguna fila

              --INSERT INTO TRAZA VALUES ('INSERT['||v_contador||':'||v_c_tarjeta_empleados.CODIGO_CARNET||']',SYSDATE);v_contador:=v_contador+1;

              IF(v_c_tarjeta_empleados.MOTIVO_ULT_CAMBIO IS NOT NULL)THEN--Tarjeta extraviada, deteriorada (hay que borrar la tarjeta anterior antes de insertar la nueva)
                DELETE FROM TARJETA_EMPLEADOS WHERE substr(CODIGO_CARNET,5,8)=substr(v_c_tarjeta_empleados.CODIGO_CARNET,5,8);
              END IF;

              INSERT INTO TARJETA_EMPLEADOS (CODIGO_CARNET,
                                             TIPO_DTO,
                                             MOTIVO_ULT_CAMBIO,
                                             DTO_EN_LIMITE,
                                             NOMBRE_EMPLEADO,
                                             LIMITE_S,
                                             DNI_EMPLEADO,
                                             DNI_FAMILIAR)
              VALUES (v_c_tarjeta_empleados.CODIGO_CARNET,
                      v_c_tarjeta_empleados.TIPO_DTO,
                      v_c_tarjeta_empleados.MOTIVO_ULT_CAMBIO,
                      v_c_tarjeta_empleados.DTO_EN_LIMITE,
                      NORMALIZAR_TEXTO(v_c_tarjeta_empleados.NOMBRE_EMPLEADO),
                      TO_NUMBER(REPLACE(v_c_tarjeta_empleados.LIMITE_S,'+',''))/100 ,
                      to_number(replace(translate(UPPER(v_c_tarjeta_empleados.DNI_EMPLEADO),'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ.-',' '),' ','')),
                      to_number(replace(translate(UPPER(v_c_tarjeta_empleados.DNI_FAMILIAR),'ABCDEFGHIJKLMNÑOPQRSTUVWXYZ.-',' '),' ','')));
            END IF;
          END IF;

          EXCEPTION WHEN OTHERS THEN --Entrará aqui cuando haya habido un error en el insert
          anotar_TRAZA ('Ha habido un error al intentar insertar el empleado '||v_c_tarjeta_empleados.NOMBRE_EMPLEADO||' con codigo_carnet '||v_c_tarjeta_empleados.CODIGO_CARNET,'CARGADATOS_MNG2.CARGA_TARJETA_EMPLEADOS',60);
        END;--del try(BEGIN-EXCEPTION)

      END LOOP;

    END Carga_TARJETA_EMPLEADOS;


  PROCEDURE Carga_Real_Picking_001 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '001'
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_realpicking IN c_realpicking
      LOOP
        SELECT P.base INTO BASE_ITEM FROM productos P WHERE P.id=v_c_realpicking.ITEM;

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR,-- --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND GETARTICULOBASE2(ARTICULO) = BASE_ITEM|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS  NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END Carga_Real_Picking_001;

  PROCEDURE load_real_picking_001 IS
    /*
        PROCEDURE NUEVO. INFORMATICA NOS ENVIA LA REFERENCIA QUE SE LE ENVIO EN EL PEDIDO ORIGINAL.( ITEM_ANTIGUO)
         COMO EN LOS PEDIDOS VOTF NO TRABAJAN CON BASES, INFORMATICA NOS ENVIA LA REFERENCIA ORIGINAL DEL PEDIDO PARA PODER ACTUALIZAR LAS LINEAS DE PEDIDO.
     */
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '001'
      ORDER BY PEDIDO;

    BEGIN
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR,-- --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND ARTICULO = v_c_realpicking.ITEM_ANTIGUO|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS  NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END load_real_picking_001;

  PROCEDURE Carga_Real_Picking_400 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '400'
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_realpicking IN c_realpicking
      LOOP
        SELECT P.base INTO BASE_ITEM FROM productos P WHERE P.id=v_c_realpicking.ITEM;

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = '40'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND GETARTICULOBASE2(ARTICULO) = BASE_ITEM|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = '40'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END Carga_Real_Picking_400;

  PROCEDURE load_real_picking_400 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '400'
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_realpicking IN c_realpicking
      LOOP

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = '40'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND ARTICULO = v_c_realpicking.ITEM_ANTIGUO|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = '40'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END load_real_picking_400;


  PROCEDURE load_real_picking_052 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '052'
      ORDER BY PEDIDO;

    BEGIN
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'TR'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND ARTICULO = v_c_realpicking.ITEM_ANTIGUO|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'TR'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END load_real_picking_052;


  PROCEDURE Carga_Real_Picking_052 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '052'
      ORDER BY PEDIDO;

    BEGIN
      FOR v_c_realpicking IN c_realpicking
      LOOP

        SELECT P.base INTO BASE_ITEM FROM productos P WHERE P.id=v_c_realpicking.ITEM;

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'TR'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND GETARTICULOBASE2(ARTICULO) = BASE_ITEM|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'TR'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END Carga_Real_Picking_052;


  PROCEDURE load_real_picking_075 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '075'
      ORDER BY PEDIDO;

    BEGIN
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'RU'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND ARTICULO = v_c_realpicking.ITEM_ANTIGUO|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'RU'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END load_real_picking_075;


  PROCEDURE Carga_Real_Picking_075 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '075'
      ORDER BY PEDIDO;

    BEGIN
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'RU'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND GETARTICULOBASE2(ARTICULO) = BASE_ITEM|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'RU'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END Carga_Real_Picking_075;

  PROCEDURE Carga_Real_Picking_720 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '720'
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_realpicking IN c_realpicking
      LOOP

        SELECT P.base INTO BASE_ITEM FROM productos P WHERE P.id=v_c_realpicking.ITEM;

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'CN'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND GETARTICULOBASE2(ARTICULO) = BASE_ITEM|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido
      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking
      LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'CN'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END Carga_Real_Picking_720;


  PROCEDURE load_real_picking_720 IS
    BASE_ITEM VARCHAR2(8);

    CURSOR c_realpicking IS
      SELECT *
      FROM MNGDES2.MNG_REAL_PICKING
      WHERE CLIENTE = '720'
      ORDER BY PEDIDO;

    BEGIN

      FOR v_c_realpicking IN c_realpicking
      LOOP

        UPDATE LINEAS_PEDIDO SET
          ARTICULO = v_c_realpicking.ITEM || v_c_realpicking.TALLA || v_c_realpicking.COLOR, --Guardamos el articulo correcto que se ha hecho picking
          FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')--guardamos esta fecha para saber que este registro se ha actualizado
        WHERE PEDIDO = 'CN'||substr(v_c_realpicking.PEDIDO,1,6)
              AND substr(v_c_realpicking.PEDIDO,7,8) = '00'--comprobamos que el pedido sea el inicial y no un subpedido 01,02,03,...
              AND ARTICULO = v_c_realpicking.ITEM_ANTIGUO|| v_c_realpicking.TALLA||v_c_realpicking.COLOR
              AND FECHA_BAJA IS NULL -- Para que no setee los items iguales que ya se han seteado
              AND ROWNUM < 2; --Para que solo setee 1 item por si mas de 1 item igual en el pedido


      END LOOP;

      --updatemos esta fecha a null
      FOR v_c_realpicking IN c_realpicking LOOP
        UPDATE LINEAS_PEDIDO SET FECHA_BAJA = NULL
        WHERE FECHA_BAJA = TO_DATE('01/01/9999','DD/MM/YYYY')
              AND PEDIDO = 'CN'||substr(v_c_realpicking.PEDIDO,1,6);
      END LOOP;

      COMMIT;

    END  load_real_picking_720;
END;
