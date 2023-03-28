CREATE TABLE cells (
    id UUID DEFAULT uuid_generate_v4() PRIMARY KEY NOT NULL,
    quest_id UUID NOT NULL,
    cell_position INT NOT NULL,
    direction TEXT NOT NULL,
    assumption BOOLEAN DEFAULT true NOT NULL
);
CREATE INDEX cells_quest_id_index ON cells (quest_id);
ALTER TABLE cells ADD CONSTRAINT cells_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE NO ACTION;
