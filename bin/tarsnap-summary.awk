BEGIN {
    time_fmt = "%a %b %e %H:%M:%S %Z %Y";
    backup_period = 24; # In hours
}
/Total size/ {
    isotime = $1;
    split(isotime, dt, "T");

    split(dt[0], dateparts)

    tstr = dt[1];
    gsub(/-/, " ", tstr);

    dstr = substr(dt[2], 0, index(dt[2], "-")-1);
    gsub(/:/, " ", dstr);

    dtstr = tstr " " dstr;
    backup_time = mktime(dtstr);
}
/All/ {
    label = "All archives";
    line = substr(substr($0, index($0, label)), length(label)+1);

    split(line, parts, " ");
    all_size = parts[1] parts[2] " " parts[3] parts[4];
}
/This/ {
    label = "This archive";
    line = substr(substr($0, index($0, label)), length(label)+1);

    split(line, parts, " ");
    this_size = parts[1] parts[2] " " parts[3] parts[4];
}
/New/ {
    label = "New data";
    line = substr(substr($0, index($0, label)), length(label)+1);

    split(line, parts, " ");
    new_size = parts[1] parts[2] " " parts[3] parts[4];
}
END {
    now = systime();
    backup_time_str = strftime(time_fmt, backup_time);
    printf("%s backup at -- %s\n", bname, backup_time_str);

    day_len_secs = 60 * 60 * backup_period;
    if ((now - backup_time) > day_len_secs) {
        printf("\033[1;31mwarning\033[0m -- \033[1;31mlast %s backup over %d hr ago\033[0m\n", bname, backup_period);
    }

    printf("%s all size -- %s\n", bname, this_size);
    printf("%s last size -- %s\n", bname, this_size);
    printf("%s new data -- %s\n", bname, new_size);
}
