const { exec } = require("child_process");

let a = Array.from({ length: 12 });

async function runScript() {
  let res = await exec(
    "Rscript /home/robin/projects/dst/2023/JAN/warmesNeujahr/analysis/downloadDailyMax.R",
    (error, stdout, stderr) => {
      if (error) {
        console.log(`error: ${error.message}`);
        return;
      }
      if (stderr) {
        console.log(`stderr: ${stderr}`);
        return;
      }
      console.log(`stdout: ${stdout}`);
    }
  );
}

for (let p of a) {
  runScript();
  console.log("run");
}
