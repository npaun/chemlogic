<?php

function magicround($num,$sf)
	{
		if ($num == 0)
		  return(0);

		$d = ceil(log10(abs($num)));
		echo("D is $d\n");
		$power = $sf - $d;
		$magnitude = pow(10,$power);
		$shifted = round($num*$magnitude);
		return($shifted/$magnitude);
}

function sf_print($num,$sf)
 {
   $rounded = magicround($num,$sf);
   if (strpos($rounded,".") !== FALSE)
    {
      $current_sf = strlen($rounded) - 1;
      $missing_sf = $sf - $current_sf;
      if ($missing_sf > 0)
      $ans = $rounded . str_repeat("0",$missing_sf);
      else
      $ans = $rounded;
    }
    else
     {
       $current_sf = strlen($rounded);
       $missing_sf = $sf - $current_sf;
       if ($missing_sf > 0)
       $ans = $rounded . "." . str_repeat("0",$missing_sf);
       else
       $ans = $rounded;
      }

      return($ans);
      }



echo(sf_print($argv[1],$argv[2])."\n");

