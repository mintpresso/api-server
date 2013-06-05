# --- !Ups
ALTER TABLE pointTypes ADD UNIQUE ( name )
ALTER TABLE points ADD INDEX ( accoundId )

# --- !Downs
ALTER TABLE pointTypes DROP INDEX ( name )
ALTER TABLE points DROP INDEX ( accoundId )