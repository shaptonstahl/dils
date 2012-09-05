<?php

define("CHECKOUTPASSWORD", 'ipotqqwwdcfokfnrllsvowifkhhtlthq');

include('minion_functions.php');

$db = mysql_connect("localhost", "minionuser", "dreres7azaxugaCa");
mysql_select_db("srh", $db);

if ($_POST['pw'] == CHECKOUTPASSWORD) {
  $row = checkout_workunit($_POST['client']);
  if( 0 == $row ) {
    echo "Hello, world!";
  } else {
  echo "minion.workunit <- " . $row['id'] . "\n" .
    "project.workunit <- " . $row['workunit'] . "\n" .
    "url.project.batch <- '" . $row['UrlProjectBatch'] . "'";
  }
} else {
  echo "Hello, world!";
}
?>
