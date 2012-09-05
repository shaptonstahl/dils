<?php

include('minion_functions.php');

// R code to generate passwords: paste(sample(letters, 32, replace=TRUE), collapse="")
// Replace passwords here and in batch.R and CHECKOUTPASSWORD (not MySQL password) in checkout.php

define("CHECKOUTPASSWORD", 'ipotqqwwdcfokfnrllsvowifkhhtlthq');
$db = mysql_connect("localhost", "minionuser", "dreres7azaxugaCa");
mysql_select_db("srh", $db);
$save_to_folder = '/home/srh/www/sheer/projects/minion_output/';

if ($_POST['pw'] == CHECKOUTPASSWORD) {

  // See if failure being reported
  if('failure' == $_POST['status']) {
    return_workunit($_POST['minionworkunit'], $db);
    echo "success";
  }
  if ('success' == $_POST['status']) {
    $checkin_result = checkin_workunit($_POST['minionworkunit'], $_POST['client'], $db);
    
    switch($checkin_result) {
      case 1:
        // One row updated
        $file_to_save = $save_to_folder . $_POST['saveto'];
        if (move_uploaded_file($_FILES['uploadedfile']['tmp_name'], $file_to_save)) echo "success";
        else echo "error";
        break;
      case 0:
        // Zero rows updated
        echo "error";
        break;
      case -1:
        // Actually this is a MySQL failure
        echo "success";
        break;
    }
  }
} else {
  echo "Howdy!";
}

?>
