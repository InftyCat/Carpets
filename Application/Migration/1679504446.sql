DROP TABLE quests;
DROP TABLE worlds;
CREATE TABLE Worlds (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    name TEXT NOT NULL,
    coloumns INT NOT NULL,
    nodenumber INT NOT NULL,
    allowednodes INT[] DEFAULT '{}' NOT NULL
);
