# --- !Ups
CREATE TABLE `accounts` (
  `id` bigint(20) NOT NULL PRIMARY KEY AUTO_INCREMENT,
  `email` varchar(200) NOT NULL UNIQUE,
  `password` varchar(1000) NOT NULL,
  `name` varchar(1000) NOT NULL
);

# --- !Downs
DROP TABLE IF EXISTS `accounts`;