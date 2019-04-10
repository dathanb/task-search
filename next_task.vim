"if exists("g:loaded_tasksearch") || &cp
"  finish
"endif
"let g:loaded_tasksearch = 1


function GoToNextTask()
  " Get the current byte offset
  let byte_offset=line2byte(line('.'))+col('.')-2
  " Get the contents of the current buffer
  " TODO: this is hacky and may work incorrectly with other line endings. If we notice issues, pick another approach
  let buf=join(getline(1, '$'), "\n")
  " Find the location of the next task
  let new_offset=system("python ~/dev/task-search/task_search.py " . byte_offset, buf)
  " Position the cursor at the next task
  exec "goto " . (new_offset + 1)
endfunction

nmap <leader>n :call GoToNextTask()<CR>

