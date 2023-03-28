ALTER TABLE cells DROP CONSTRAINT cells_ref_quest_id;
ALTER TABLE sessions DROP CONSTRAINT sessions_ref_quest_id;
ALTER TABLE cells ADD CONSTRAINT cells_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE CASCADE;
ALTER TABLE sessions ADD CONSTRAINT sessions_ref_quest_id FOREIGN KEY (quest_id) REFERENCES quests (id) ON DELETE CASCADE;
