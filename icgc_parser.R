library(RCurl)
library(jsonlite)

icgc_query = function(q, type) {
    cmd = sprintf("curl -X GET --header 'Accept: application/json' 'https://dcc.icgc.org/api/v1/keywords?q=%s&type=%s&filters=%%7B%%7D&from=1&size=1'", q, type)
    this_query = fromJSON(system(cmd, intern = T))$hits
    if (is.list(this_query) & length(this_query) == 0) {
        return(NULL)
    } else {
        return(this_query)
    }
}

parse4tcga = function(q_df, as.data.table = TRUE) {
    if (is.null(q_df)) {
        return(NULL)
    }
    rownames(q_df) = NULL
    submittedSampleIds = q_df$submittedSampleIds[[1]]
    submittedSpecimenIds = split_conc(submittedSampleIds, 1:4, split = "-")
    sampleIds = q_df$sampleIds[[1]]
    tmp_df1 = data.frame(specimenIds = q_df$specimenIds[[1]], submittedSpecimenIds = q_df$submittedSpecimenIds[[1]], stringsAsFactors = FALSE)
    tmp_df2 = data.frame(submittedSampleIds, submittedSpecimenIds, sampleIds, stringsAsFactors = FALSE)
    tmp_df = merge(tmp_df1, tmp_df2, by = "submittedSpecimenIds")
    return_df = cbind(q_df[, c('id', 'type', 'projectId', 'submittedId')], tmp_df)
    if (as.data.table) {
        return(as.data.table(return_df))
    } else {
        return(return_df)
    }
}

tcga_bcr2icgc = function(barcodes, type = "submittedSampleIds", mc.cores = 1, as.data.table = TRUE) {
    tbl_lst = mclapply(mc.cores = mc.cores, X = selfname(barcodes), FUN = function(q) {
        try({
            parent_env = parent.env(environment())
            q_df = icgc_query(q, type = type)
            if (is.null(q_df)) {
                return(NULL)
            }
            id_tbl = parse4tcga(q_df, as.data.table = FALSE) ## this is purposeful
            id_tbl = id_tbl[id_tbl[[parent_env$type]] == q,]
            if (as.data.table) {
                return(as.data.table(id_tbl))
            } else {
                return(id_tbl)
            }
        })
    })
    empties = which(elementNROWS(tbl_lst) == 0)
    absent_bcr = names(tbl_lst[empties])
    pcawg_tcga_map_dt = rbindlist(tbl_lst)
    pcawg_tcga_map_dt = setattr(pcawg_tcga_map_dt, "absent_bcr", absent_bcr)
    return(pcawg_tcga_map_dt)
}



parse4pcawg = function(q_df, as.data.table = TRUE) {
    if (is.null(q_df)) {
        return(NULL)
    }
    rownames(q_df) = NULL
    return_df = q_df[, c("id", "type", "projectId", "submittedId")] ## i just want to know be able to map icgc donor id to tcga cases.submitter_id, all other info is redundant (we have already in some from)
    if (as.data.table) {
        return(as.data.table(return_df))
    } else {
        return(return_df)
    }
}

icgc2tcga = function(icgc_donor_id, type = "donorId", mc.cores = 1, as.data.table = TRUE) {
    tbl_lst = mclapply(mc.cores = mc.cores, X = selfname(icgc_donor_id), FUN = function(q) {
        try({
            parent_env = parent.env(environment())
            q_df = icgc_query(q, type = type)
            if (is.null(q_df)) {
                return(NULL)
            }   
            id_tbl = parse4pcawg(q_df, as.data.table = FALSE) ## this is purposeful
            if (parent_env$type == "donorId") {
                id_tbl = id_tbl[id_tbl[["id"]] == q]
            } else {
                id_tbl = id_tbl[id_tbl[[parent_env$type]] == q,]
            }            
            if (as.data.table) {
                return(as.data.table(id_tbl))
            } else {
                return(id_tbl)
            }
        })
    })
    pcawg_map = rbindlist(tbl_lst)
    return(pcawg_map)
}
