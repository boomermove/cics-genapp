-- ================================================================
-- Licensed Materials - Property of IBM                          
-- GENAPP - CICS GenApp Security Tables DDL                     
-- (C) Copyright IBM Corp. 2023. All Rights Reserved             
-- US Government Users Restricted Rights - Use, duplication or    
-- disclosure restricted by GSA ADP Schedule Contract with IBM    
-- Corp                                                           
-- ================================================================

-- ================================================================
-- USER_SECURITY Table - Stores secure user authentication data
-- ================================================================
CREATE TABLE USER_SECURITY (
    CUSTOMERNUM     INTEGER       NOT NULL,
    USERNAME        VARCHAR(32)   NOT NULL,
    PASSWORD_HASH   VARCHAR(128)  NOT NULL,
    SALT           VARCHAR(32)   NOT NULL,
    HASH_ALGORITHM  VARCHAR(8)    NOT NULL DEFAULT 'SHA256',
    PASSWORD_DATE   DATE          NOT NULL DEFAULT CURRENT DATE,
    LAST_LOGIN      TIMESTAMP,
    LOGIN_ATTEMPTS  SMALLINT      NOT NULL DEFAULT 0,
    ACCOUNT_STATUS  CHAR(1)       NOT NULL DEFAULT 'A',
    LOCKOUT_TIME    TIMESTAMP,
    CREATED_DATE    DATE          NOT NULL DEFAULT CURRENT DATE,
    MODIFIED_DATE   DATE          NOT NULL DEFAULT CURRENT DATE,
    CONSTRAINT PK_USER_SECURITY PRIMARY KEY (CUSTOMERNUM),
    CONSTRAINT UK_USERNAME UNIQUE (USERNAME),
    CONSTRAINT FK_USER_CUSTOMER 
        FOREIGN KEY (CUSTOMERNUM) REFERENCES CUSTOMER(CUSTOMERNUMBER),
    CONSTRAINT CK_ACCOUNT_STATUS 
        CHECK (ACCOUNT_STATUS IN ('A', 'L', 'S', 'E')),
    CONSTRAINT CK_LOGIN_ATTEMPTS 
        CHECK (LOGIN_ATTEMPTS >= 0 AND LOGIN_ATTEMPTS <= 99)
);

-- Create indexes for performance
CREATE INDEX IX_USER_SECURITY_USERNAME ON USER_SECURITY (USERNAME);
CREATE INDEX IX_USER_SECURITY_STATUS ON USER_SECURITY (ACCOUNT_STATUS);
CREATE INDEX IX_USER_SECURITY_MODIFIED ON USER_SECURITY (MODIFIED_DATE);

-- ================================================================
-- PASSWORD_HISTORY Table - Stores password history for users
-- ================================================================
CREATE TABLE PASSWORD_HISTORY (
    CUSTOMERNUM     INTEGER       NOT NULL,
    SEQUENCE        SMALLINT      NOT NULL,
    PASSWORD_HASH   VARCHAR(128)  NOT NULL,
    CREATED_DATE    DATE          NOT NULL DEFAULT CURRENT DATE,
    CONSTRAINT PK_PASSWORD_HISTORY 
        PRIMARY KEY (CUSTOMERNUM, SEQUENCE),
    CONSTRAINT FK_PWHISTORY_USER 
        FOREIGN KEY (CUSTOMERNUM) REFERENCES USER_SECURITY(CUSTOMERNUM),
    CONSTRAINT CK_SEQUENCE 
        CHECK (SEQUENCE >= 1 AND SEQUENCE <= 10)
);

-- ================================================================
-- AUDIT_LOG Table - Security audit logging
-- ================================================================
CREATE TABLE AUDIT_LOG (
    LOG_ID          INTEGER       NOT NULL GENERATED ALWAYS AS IDENTITY,
    TIMESTAMP       TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    CUSTOMER_NUM    INTEGER,
    USERNAME        VARCHAR(32),
    ACTION          VARCHAR(20)   NOT NULL,
    RESULT          CHAR(2)       NOT NULL,
    CLIENT_IP       VARCHAR(15),
    USER_AGENT      VARCHAR(100),
    ERROR_CODE      VARCHAR(10),
    DETAILS         VARCHAR(200),
    CONSTRAINT PK_AUDIT_LOG PRIMARY KEY (LOG_ID)
);

-- Create indexes for audit queries
CREATE INDEX IX_AUDIT_TIMESTAMP ON AUDIT_LOG (TIMESTAMP);
CREATE INDEX IX_AUDIT_CUSTOMER ON AUDIT_LOG (CUSTOMER_NUM);
CREATE INDEX IX_AUDIT_ACTION ON AUDIT_LOG (ACTION);
CREATE INDEX IX_AUDIT_RESULT ON AUDIT_LOG (RESULT);

-- ================================================================
-- SESSION_TOKENS Table - Active user sessions
-- ================================================================
CREATE TABLE SESSION_TOKENS (
    SESSION_ID      VARCHAR(64)   NOT NULL,
    CUSTOMERNUM     INTEGER       NOT NULL,
    USERNAME        VARCHAR(32)   NOT NULL,
    CREATED_TIME    TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    EXPIRY_TIME     TIMESTAMP     NOT NULL,
    CLIENT_IP       VARCHAR(15),
    USER_AGENT      VARCHAR(100),
    LAST_ACTIVITY   TIMESTAMP     NOT NULL DEFAULT CURRENT TIMESTAMP,
    STATUS          CHAR(1)       NOT NULL DEFAULT 'A',
    CONSTRAINT PK_SESSION_TOKENS PRIMARY KEY (SESSION_ID),
    CONSTRAINT FK_SESSION_USER 
        FOREIGN KEY (CUSTOMERNUM) REFERENCES USER_SECURITY(CUSTOMERNUM),
    CONSTRAINT CK_SESSION_STATUS 
        CHECK (STATUS IN ('A', 'E', 'I'))
);

-- Create indexes for session management
CREATE INDEX IX_SESSION_CUSTOMER ON SESSION_TOKENS (CUSTOMERNUM);
CREATE INDEX IX_SESSION_EXPIRY ON SESSION_TOKENS (EXPIRY_TIME);
CREATE INDEX IX_SESSION_STATUS ON SESSION_TOKENS (STATUS);

-- ================================================================
-- Security Configuration Table
-- ================================================================
CREATE TABLE SECURITY_CONFIG (
    CONFIG_KEY      VARCHAR(50)   NOT NULL,
    CONFIG_VALUE    VARCHAR(200)  NOT NULL,
    DESCRIPTION     VARCHAR(500),
    MODIFIED_DATE   DATE          NOT NULL DEFAULT CURRENT DATE,
    MODIFIED_BY     VARCHAR(32)   NOT NULL DEFAULT USER,
    CONSTRAINT PK_SECURITY_CONFIG PRIMARY KEY (CONFIG_KEY)
);

-- Insert default security configuration
INSERT INTO SECURITY_CONFIG VALUES
('HASH_ALGORITHM', 'SHA256', 'Default password hashing algorithm', CURRENT DATE, USER),
('MIN_PASSWORD_LENGTH', '8', 'Minimum password length', CURRENT DATE, USER),
('MAX_PASSWORD_LENGTH', '32', 'Maximum password length', CURRENT DATE, USER),
('PASSWORD_EXPIRY_DAYS', '90', 'Password expiration in days', CURRENT DATE, USER),
('MAX_LOGIN_ATTEMPTS', '3', 'Maximum failed login attempts', CURRENT DATE, USER),
('LOCKOUT_DURATION', '1800', 'Account lockout duration in seconds', CURRENT DATE, USER),
('SESSION_TIMEOUT', '3600', 'Session timeout in seconds', CURRENT DATE, USER),
('AUDIT_LEVEL', 'A', 'Audit level (A=All, F=Failures, N=None)', CURRENT DATE, USER),
('REQUIRE_UPPERCASE', 'Y', 'Require uppercase in password', CURRENT DATE, USER),
('REQUIRE_LOWERCASE', 'Y', 'Require lowercase in password', CURRENT DATE, USER),
('REQUIRE_DIGIT', 'Y', 'Require digit in password', CURRENT DATE, USER),
('REQUIRE_SPECIAL', 'N', 'Require special character in password', CURRENT DATE, USER),
('PASSWORD_HISTORY_COUNT', '5', 'Number of previous passwords to remember', CURRENT DATE, USER),
('COMPLEXITY_SCORE', '60', 'Minimum password complexity score', CURRENT DATE, USER);

-- ================================================================
-- Views for reporting and monitoring
-- ================================================================

-- Active users view
CREATE VIEW ACTIVE_USERS AS
SELECT u.CUSTOMERNUM, u.USERNAME, u.LAST_LOGIN, u.ACCOUNT_STATUS,
       u.LOGIN_ATTEMPTS, u.CREATED_DATE
FROM USER_SECURITY u
WHERE u.ACCOUNT_STATUS = 'A';

-- Locked accounts view  
CREATE VIEW LOCKED_ACCOUNTS AS
SELECT u.CUSTOMERNUM, u.USERNAME, u.LOCKOUT_TIME, u.LOGIN_ATTEMPTS,
       u.MODIFIED_DATE
FROM USER_SECURITY u
WHERE u.ACCOUNT_STATUS = 'L';

-- Recent audit activity view
CREATE VIEW RECENT_AUDIT AS
SELECT LOG_ID, TIMESTAMP, CUSTOMER_NUM, USERNAME, ACTION, RESULT,
       CLIENT_IP, ERROR_CODE
FROM AUDIT_LOG
WHERE TIMESTAMP >= CURRENT TIMESTAMP - 24 HOURS
ORDER BY TIMESTAMP DESC;

-- Active sessions view
CREATE VIEW ACTIVE_SESSIONS AS
SELECT s.SESSION_ID, s.CUSTOMERNUM, s.USERNAME, s.CREATED_TIME,
       s.EXPIRY_TIME, s.LAST_ACTIVITY, s.CLIENT_IP
FROM SESSION_TOKENS s
WHERE s.STATUS = 'A' AND s.EXPIRY_TIME > CURRENT TIMESTAMP;

-- ================================================================
-- Stored procedures for common operations
-- ================================================================

-- Procedure to cleanup expired sessions
CREATE OR REPLACE PROCEDURE CLEANUP_EXPIRED_SESSIONS()
LANGUAGE SQL
BEGIN
    DELETE FROM SESSION_TOKENS 
    WHERE EXPIRY_TIME < CURRENT TIMESTAMP OR STATUS = 'E';
    
    INSERT INTO AUDIT_LOG (CUSTOMER_NUM, USERNAME, ACTION, RESULT, DETAILS)
    VALUES (0, 'SYSTEM', 'CLEANUP_SESSIONS', '00', 
            'Expired sessions cleanup completed');
END;

-- Procedure to reset failed login attempts
CREATE OR REPLACE PROCEDURE RESET_LOGIN_ATTEMPTS(IN p_customernum INTEGER)
LANGUAGE SQL
BEGIN
    UPDATE USER_SECURITY 
    SET LOGIN_ATTEMPTS = 0, MODIFIED_DATE = CURRENT DATE
    WHERE CUSTOMERNUM = p_customernum;
    
    INSERT INTO AUDIT_LOG (CUSTOMER_NUM, ACTION, RESULT, DETAILS)
    VALUES (p_customernum, 'RESET_ATTEMPTS', '00', 
            'Login attempts reset by administrator');
END;

-- Grant appropriate permissions
GRANT SELECT, INSERT, UPDATE ON USER_SECURITY TO GENAPP;
GRANT SELECT, INSERT ON PASSWORD_HISTORY TO GENAPP;
GRANT INSERT ON AUDIT_LOG TO GENAPP;
GRANT SELECT, INSERT, UPDATE, DELETE ON SESSION_TOKENS TO GENAPP;
GRANT SELECT ON SECURITY_CONFIG TO GENAPP;
GRANT SELECT ON ACTIVE_USERS TO GENAPP;
GRANT SELECT ON LOCKED_ACCOUNTS TO GENAPP;
GRANT SELECT ON RECENT_AUDIT TO GENAPP;
GRANT SELECT ON ACTIVE_SESSIONS TO GENAPP;

-- Create comments on tables
COMMENT ON TABLE USER_SECURITY IS 
'Secure user authentication data - replaces hardcoded credentials';
COMMENT ON TABLE PASSWORD_HISTORY IS 
'Password history to prevent reuse of recent passwords';
COMMENT ON TABLE AUDIT_LOG IS 
'Security audit log for authentication events';
COMMENT ON TABLE SESSION_TOKENS IS 
'Active user session management';
COMMENT ON TABLE SECURITY_CONFIG IS 
'Configurable security parameters';

COMMIT;