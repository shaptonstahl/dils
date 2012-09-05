<?php

/* functions
 
 add_workunits: Add an array of workunits to a project
 checkin_workunit: 
 checkout_workunit: 
 is_checked_in: 
 is_checked_out: 
 workunit_exists: 
 
*/

function add_workunits($project, $workunits) {
  /* Add an array of workunits to a project */
  foreach($workunits as $workunit) {
    if( 0 == workunit_exists($project, $workunit) ) {
      $SQL = "INSERT INTO `workunits` (`project`, `workunit`) VALUES ('" . $project . "', '" . $workunit . "')";
      $sql_result = mysql_query($SQL);
    }
  }
}

function checkin_workunit($project, $workunit, $dblink) {
  /* Return 1 if successful
            0 if failed; this includes if project/workunit pair is invalid
           -1 if already checked in
  */
  if ( 1 == is_checked_in($project, $workunit) ) {
    return(-1);
  } else {
    $SQL_update = "UPDATE `workunits` SET `checkedout`=1, `checkedin`=1 WHERE `project`='" . 
      $project . "' AND `workunit`='" . $workunit . "' LIMIT 1";
    $sql_result = mysql_query($SQL_update, $dblink);
    return( mysql_affected_rows($dblink) );
  }
}

function checkout_workunit($project) {
  /* Retrieve a work unit that is not checked in.
     Choose the one that was modified least recently.
     Mark it as checked out.
  */
  $SQL = "SELECT * FROM `workunits` WHERE `project`='" . $project . "' AND `checkedin`=0 ORDER BY `timestamp`,`id` LIMIT 1";
  $sql_result = mysql_query($SQL);
  if (mysql_num_rows($sql_result) == 0) {
    /* No work units available */
    return(0);
  } else {
    $row = mysql_fetch_array($sql_result);
    $SQL_update = "UPDATE `workunits` SET `timestamp`=NOW(),`checkedout`=1 WHERE `id`=" . $row['id'] . " LIMIT 1";
    mysql_query($SQL_update);
    return($row['workunit']);
  }
}

function current_projects() {
  $SQL = "SELECT DISTINCTROW `project` as p, (SELECT max(`timestamp`) FROM `workunits` WHERE `project`=p) as latest FROM `workunits` ORDER BY latest DESC";
  $SQL_projects = mysql_query($SQL);
  while($row = mysql_fetch_array($SQL_projects)) $out[$row['p']] = $row['latest'];
  return($out);
}

function is_checked_in($project, $workunit) {
  /* Return 1 if checked in
            0 if not; this includes if project/workunit pair is invalid
  */
  $SQL = "SELECT * FROM `workunits` WHERE `project`='" . $project . "' AND `workunit`='" . 
    $workunit . "' AND `checkedin`=1 LIMIT 1";
  $sql_result = mysql_query($SQL);
  return(mysql_num_rows($sql_result));
}

function is_checked_out($project, $workunit) {
  /* Return 1 if checked in
            0 if not; this includes if project/workunit pair is invalid
  */
  $SQL = "SELECT * FROM `workunits` WHERE `project`='" . $project . "' AND `workunit`='" . 
    $workunit . "' AND `checkedout`=1 LIMIT 1";
  $sql_result = mysql_query($SQL);
  return(mysql_num_rows($sql_result));
}

function workunit_exists($project, $workunit) {
  /* Return 1 if workunit exists for this project
            0 if not
  */
  $SQL = "SELECT * FROM `workunits` WHERE `project`='" . $project . "' AND `workunit`='" . $workunit . "' LIMIT 1";
  $sql_result = mysql_query($SQL);
  return(mysql_num_rows($sql_result));
}

?>
