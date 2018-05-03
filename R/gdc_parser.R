library(RCurl)
library(jsonlite)
library(XML)

gdc_end = function(endpoint, legacy = FALSE, version = NULL) {
    if (!is.null(version)) {
        version = paste0("", version, "/")
    } else {
        version = ""
    }
    if (legacy) {
        paste0("https://api.gdc.cancer.gov/", version, "legacy/", endpoint)
    } else  {
        paste0("https://api.gdc.cancer.gov/", version, endpoint)
    }
}


gdc_filt = function(filters) {
    library(RCurl)
    library(jsonlite)
    curlEscape(toJSON(filters, auto_unbox = T))
}




gdc_url = function(end = "cases", legacy = FALSE, filters = NULL, cust_ins = "", extras = "") {
    if (end %in% c("data", "manifest")) {
        if (length(cust_ins > 1)) {
            cust_ins = paste0(cust_ins, collapse = ",")
        }
        return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins))
        } else {
    }
    if (!is.null(filters)) {
        if (nchar(extras) == 0) {
            return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters), "&pretty=true"))
        } else {
            return(paste0(gdc_end(end, legacy = legacy), "?filters=", gdc_filt(filters), "&pretty=true", extras))
        }
    } else {
        if (nchar(extras) == 0) {
            return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins, "&pretty=true"))
        } else {
            return(paste0(gdc_end(end, legacy = legacy), "/", cust_ins, extras, "&pretty=true"))
        }
    }
}


fields_example = "files.experimental_strategy,files.file_name,files.file_id,files.center.short_name,files.data_type,files.data_category,files.metadata_files.file_name"


gdccurl = function(url, fields = "", download = FALSE, download_dir = NULL, size = NULL) {
    if (!download) {
        cmd = paste0("curl ", "'", url)
        if (nchar(fields) > 0) {
            cmd = paste0(cmd, "&fields=", fields)
        }
        if (!is.null(size)) {
            cmd = paste0(cmd, sprintf("&size=%s", size))
        }
        cmd = paste0(cmd, "'")
        return(fromJSON(system(cmd, intern = T)))
    } else if (grepl("\\/data\\/|manifest", url)) {
        if (!is.null(download_dir)) {
            tmp.env123987  = new.env()
            assign("orig_wd", getwd(), envir = tmp.env123987)
            setwd(download_dir)
        }
        return(system(paste0("curl --remote-name --remote-header-name ", "'",  url, "'")))
        try(setwd(tmp.env123987$orig_wd))
    }

}



analysis_grab = function(xmllist) {
    dt = as.data.table(t(xmllist$ANALYSIS_SET$ANALYSIS$.attrs))
    label = paste(xmllist$ANALYSIS_SET$ANALYSIS$TITLE, xmllist$ANALYSIS_SET$ANALYSIS$DESCRIPTION)
    assembly = xmllist$ANALYSIS_SET$ANALYSIS$ANALYSIS_TYPE$REFERENCE_ALIGNMENT$ASSEMBLY[[1]]
    dt[, label := label]
    dt[, assembly := assembly]
    lst_of_pipelines = xmllist$ANALYSIS_SET$ANALYSIS$ANALYSIS_TYPE$REFERENCE_ALIGNMENT$PROCESSING$PIPELINE
    set(dt, i = 1L, j = "lst_of_pipelines", value = list(list(lst_of_pipelines)))
    starts = regexpr("TCGA\\-", label)
    clean_ident = unlist(lapply(strsplit(substring(label, starts), "\\:"), function(x) x[1]))
    legacy_sample_id = substring(clean_ident, 1, 28)
    sample = substring(legacy_sample_id, 1, 25)
    low_passes = grepl("(Low pass)|(Low Pass)", label)
    tss_id = substring(legacy_sample_id,first = 6, last = 7)
    sample_type = substring(legacy_sample_id,first = 14, last = 15)
    analyte_code = substring(legacy_sample_id,first = 20, last = 20)
    dt[, legacy_sample_id := legacy_sample_id]
    dt[, sample := sample]
    dt[, low_passes := low_passes]
    dt[, tss_id := tss_id]
    dt[, sample_type := sample_type]
    dt[, analyte_code := analyte_code]
    #' to access each entry of lst_of_pipelines dt$lst_of_pipelines[[1]]
    return(dt)
}


grab_xml = function(these_xml, mc.cores = 1, mc.preschedule = TRUE) {
    mclapply(these_xml, function(this_xml)
    {
        this = tryCatch(
        {
            tmp = xmlToList(this_xml)
            message("read in: ", this_xml, "\n")
            dt = analysis_grab(tmp)
            dt[, analysis_file_name := basename(this_xml)]
            dt[, analysis_local_path := this_xml]
            return(dt)
        }, error = function(e) "try-error")
        if (this == "try-error") {
            warning(this_xml, " has produced an error")
        }
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}


grab_by_file_id = function(file_ids, mc.cores = 1, mc.preschedule = TRUE, size = 1)
{
    mclapply(file_ids, function(ix)
    {
        tryCatch(
        {
            filters = list(op = "in", content = list(field = "files.file_id", value = ix))
            url = gdc_url("files", legacy = TRUE, filters)
            bla = gdccurl(url, size = size)
            dt = as.data.table(bla$data$hits)
            bla2 = gdccurl(url, fields = "cases.case_id,cases.submitter_id", size = size)
            dt2 = as.data.table(bla2$data$hits$cases)
            setnames(dt2, paste0("cases.", names(dt2)))
            build_q = cbind(dt, dt2)
            file_nm = build_q$file_name
            filters2 = list(op = "in", content = list(field = "files.file_name", value = file_nm))
            url2 = gdc_url("files", TRUE, filters2)
            bla2 = gdccurl(url, fields = "metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type", size = size)
            analysis_file = as.data.table(bla2$data$hits$metadata_files[[1]])[data_type == "Analysis Metadata"]
            setnames(analysis_file, paste0("analysis_", names(analysis_file)))
            analysis_file[, analysis_id := sub("\\_analysis.xml", "", analysis_file_name)]
            build_q = cbind(build_q, analysis_file)
            build_q[, id := NULL]
            return(build_q)
        }, error = function(e) "try-error")
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}


grab_by_file_name = function(file_names, mc.cores = 1, mc.preschedule = TRUE, size = 1)
{
    mclapply(file_names, function(ix)
    {
        tryCatch(
        {
            filters2 = list(op = "in", content = list(field = "files.file_name", value = ix))
            url2 = gdc_url("files", TRUE, filters2)
            bla2 = gdccurl(url2, fields = "metadata_files.file_id,metadata_files.file_name,metadata_files.data_format,metadata_files.data_category,metadata_files.data_type", size = size)
            analysis_file = as.data.table(bla2$data$hits$metadata_files[[1]])[data_type == "Analysis Metadata"]
            setnames(analysis_file, paste0("analysis_", names(analysis_file)))
            analysis_file[, analysis_id := sub("\\_analysis.xml", "", analysis_file_name)]
            file_id = bla2$data$hits$id

            filters = list(op = "in", content = list(field = "files.file_id", value = file_id))
            url = gdc_url("files", legacy = TRUE, filters)
            bla = gdccurl(url, size = size)
            dt = as.data.table(bla$data$hits)

            bla2 = gdccurl(url, fields = "cases.case_id,cases.submitter_id", size = size)
            dt2 = as.data.table(bla2$data$hits$cases)
            setnames(dt2, paste0("cases.", names(dt2)))

            build_q = cbind(dt, dt2)
            build_q = cbind(build_q, analysis_file)
            build_q[, id := NULL]
            return(build_q)
        }, error = function(e) "try-error")
    }, mc.cores = mc.cores, mc.preschedule = mc.preschedule)
}
