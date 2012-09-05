<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN"
  "http://www.w3.org/TR/xhtml1/DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en">
<head>
  <title>Batch Job Contol Panel</title>
    <style type="text/css">
    label {
      float: left;
      width: 20em;
      display: inline;
      clear: both;
      font-weight: bold;
    }
    .syllabus {
      line-height: normal;
      margin-bottom: 1em;
    }
    .syllabusContentGroup {
      font-family: serif;
      font-size: 150%;
      font-weight: 500;
    }
    .HideShowButton {
      margin-bottom: 1em;
    }
    .PreprocessingMessageBox {
      border: thick solid green;
    }
  </style>
</head>

<?php /* PHP Headers */

include('minion_functions.php');

$db = mysql_connect("localhost", "minionuser", "dreres7azaxugaCa");
mysql_select_db("srh", $db);

?>

<?php /* PREPROCESSING */

if('add_workunits' == $_POST['action']) {
  if( $_POST['add_min_workunit'] % 1 != 0 || $_POST['add_max_workunit'] % 1 != 0 ) {
    $preprocessing_messages[] = "Add workunits: min workunit and max workunit must be positive integers with max > min.";
  } else {
    for($workunit = $_POST['add_min_workunit']; $workunit <= $_POST['add_max_workunit']; $workunit++) $workunits[] = $workunit;
    add_workunits($_POST['add_project'], $_POST['add_UrlProjectBatch'], $workunits);
    $preprocessing_messages[] = "Add workunits: workunits added";
  }
}

/* Show preprocessing messages */
if( isset($preprocessing_messages) ) {
  foreach($preprocessing_messages as $this_msg) echo "<div class='PreprocessingMessageBox'>" . $this_msg . "</div>\n";
}

?>

<body>

<h1>Batch Job Contol Panel</h1>

<?php

$projects = current_projects();

echo "<table border>
  <tr style='text-align: center; font-weight: bold'><td>Project title</td><td>Latest Activity</td><td>Total workunits</td><td>Workunits in progress</td><td>Workunits complete</td><td>Projected completion</td><td>Project batch file</td></tr>\n";
foreach($projects as $project => $latest) {
  $SQL_count_all = "SELECT * FROM `minion` WHERE `project`='" . $project . "'";
  $sql_result_count_all = mysql_query($SQL_count_all);
  $count_all = mysql_num_rows($sql_result_count_all);

  $SQL_count_out_not_in = "SELECT * FROM `minion` WHERE `project`='" . $project . "' AND `checkedout`=1 AND `checkedin`=0";
  $sql_result_count_out_not_in = mysql_query($SQL_count_out_not_in);
  $count_out_not_in = mysql_num_rows($sql_result_count_out_not_in);

  $SQL_count_in = "SELECT * FROM `minion` WHERE `project`='" . $project . "' AND `checkedin`=1";
  $sql_result_count_in = mysql_query($SQL_count_in);
  $count_in = mysql_num_rows($sql_result_count_in);

  $SQL_recent = "SELECT * FROM `minion` WHERE `project`='" . $project . "' AND `checkedin`=1 AND `timestamp` > TIMESTAMPADD(MINUTE, -180, NOW())";
  $sql_result_recent = mysql_query($SQL_recent);
  $count_recent = mysql_num_rows($sql_result_recent);
  if(0 == $count_recent) {
    $projected_completion = "--";
  } else {
    $projected_completion = date("l, F j, Y, h:i a",
      strtotime("+" . round( ($count_all - $count_in) / $count_recent * 180) . " min") );
  }
  
  $row = mysql_fetch_array($sql_result_count_all);

  echo "  <tr style='text-align: center'><td>" . $project . "</td><td>" . $latest . "</td><td>" . $count_all . "</td><td>" . $count_out_not_in .
    "</td><td>" . $count_in . "</td><td>" . $projected_completion . "</td><td style='text-align: left'>" . $row['UrlProjectBatch'] . "</td></tr>\n";
}
echo "</table>\n";

?>

<form method='post' action="<?php echo $_SERVER['PHP_SELF'] ?>">
  <h2>Add Workunits</h2>
  <div><label for='add_project'>Project:</label> <input id='add_project' name='add_project'/></div>
  <div><label for='add_UrlProjectBatch'>URL for the project-specific R script:</label> <input id='add_UrlProjectBatch' name='add_UrlProjectBatch' size='80'/></div>
  <div><label for='add_min_workunit'>Min workunit:</label> <input id='add_min_workunit' name='add_min_workunit'/></div>
  <div><label for='add_max_workunit'>Max workunit:</label> <input id='add_max_workunit' name='add_max_workunit'/></div>
  <div><label for='add_submit'>&nbsp;</label> <input type='submit' id='add_submit' name='add_submit' value='Add Workunits'/></div>
  Existing workunits will not be removed or modified.
  <input type='hidden' name='action' value='add_workunits'/>
</form>

<div>View <a href='http://sheer.ucdavis.edu/projects/minion_output/'>output files</a>.</div>

<div>View <a href='do_workunit.R'>R batch file</a>.</div>

</body>
</html>
