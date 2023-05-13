CREATE TABLE public.word_rom (
	id int NOT NULL,
	word_rom text NOT NULL,
	word_eng text
);
CREATE INDEX IX_001 ON word_rom (word_rom);
CREATE INDEX IX_002 ON word_rom (word_eng);
CREATE INDEX IX_003 ON word_rom (id);


CREATE TABLE public.word_rus (
	id int NOT NULL,
	word_rus text NOT NULL
);
CREATE INDEX IX_004 ON word_rus (word_rus);
CREATE INDEX IX_005 ON word_rom (id);