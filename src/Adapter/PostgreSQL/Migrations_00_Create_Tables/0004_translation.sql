CREATE TABLE translations (
  idWord INT NOT NULL,
  idTranslation INT NOT NULL,
  CONSTRAINT IdWord_IdTranslation UNIQUE (idWord,idTranslation)
);

CREATE INDEX IX_006 ON translations (idWord);