--
-- PostgreSQL database dump
--

SET statement_timeout = 0;
SET lock_timeout = 0;
SET client_encoding = 'UTF8';
SET standard_conforming_strings = on;
SET check_function_bodies = false;
SET client_min_messages = warning;

--
-- Name: plpgsql; Type: EXTENSION; Schema: -; Owner: 
--

CREATE EXTENSION IF NOT EXISTS plpgsql WITH SCHEMA pg_catalog;


--
-- Name: EXTENSION plpgsql; Type: COMMENT; Schema: -; Owner: 
--

COMMENT ON EXTENSION plpgsql IS 'PL/pgSQL procedural language';


SET search_path = public, pg_catalog;

SET default_tablespace = '';

SET default_with_oids = false;

--
-- Name: mars_weather; Type: TABLE; Schema: public; Owner: postgres; Tablespace: 
--

CREATE TABLE mars_weather (
    terrestrial_date date NOT NULL,
    sol integer NOT NULL,
    ls double precision,
    min_temp double precision,
    min_temp_fahrenheit double precision,
    max_temp double precision,
    max_temp_fahrenheit double precision,
    pressure double precision,
    pressure_string character varying(50),
    abs_humidity double precision,
    wind_speed double precision,
    wind_direction character varying(50),
    atmo_opacity character varying(50),
    season character varying(50),
    sunrise timestamp without time zone,
    sunset timestamp without time zone
);


ALTER TABLE mars_weather OWNER TO postgres;

--
-- Data for Name: mars_weather; Type: TABLE DATA; Schema: public; Owner: postgres
--

COPY mars_weather (terrestrial_date, sol, ls, min_temp, min_temp_fahrenheit, max_temp, max_temp_fahrenheit, pressure, pressure_string, abs_humidity, wind_speed, wind_direction, atmo_opacity, season, sunrise, sunset) FROM stdin;
\.


--
-- Name: public; Type: ACL; Schema: -; Owner: postgres
--

REVOKE ALL ON SCHEMA public FROM PUBLIC;
REVOKE ALL ON SCHEMA public FROM postgres;
GRANT ALL ON SCHEMA public TO postgres;
GRANT ALL ON SCHEMA public TO PUBLIC;


--
-- Name: mars_weather; Type: ACL; Schema: public; Owner: postgres
--

REVOKE ALL ON TABLE mars_weather FROM PUBLIC;
REVOKE ALL ON TABLE mars_weather FROM postgres;
GRANT ALL ON TABLE mars_weather TO postgres;
GRANT ALL ON TABLE mars_weather TO mars;


--
-- PostgreSQL database dump complete
--

